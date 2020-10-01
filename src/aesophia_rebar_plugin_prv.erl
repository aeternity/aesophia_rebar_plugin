-module(aesophia_rebar_plugin_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, aesophia).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 aesophia --contract Staking.aes --out Staking.json\nrebar3 aesophia --contract Staking.aes --out Staking.json --verify"},
                                          % How to use the plugin
            {opts, [{contract, $c, "contract", string, "filename of the Sophia contract source code"},
                    {compile, $o, "out", string, "filename of the compilation result"},
                    {verify, $v, "verify", boolean, "filename of a compilation result to verify against the given source code"},
                    {compiler_version, $s, "version", string, "Compiler version, by default v4.3.1"}
                   ]},                    % list of options understood by the plugin
            {short_desc, "Compile sophia contracts."},
            {desc, "Compile or validate sophia contracts while building your project."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {KVs, _} = rebar_state:command_parsed_args(State),
    ContractFilename = proplists:get_value(contract, KVs, get_from_state(contract, State, undefined)),
    OutFilename = proplists:get_value(compile, KVs, get_from_state(compile, State, undefined)),
    Verify = proplists:get_value(verify, KVs, get_from_state(verify, State, false)),
    CompilerVersion = proplists:get_value(compile, KVs, get_from_state(compile, State, "v4.3.1")),
    case compiler_path(CompilerVersion, Verify) of
        {ok, CompilerPath} ->
            case {ContractFilename, OutFilename, Verify} of
                {undefined, _, _} ->
                    {error, "Please provide a sophia contract to process"};
                {_, undefined, _} ->
                    {error, "Please provide an output file"};
                {_, _, false} ->
                    compile(State, CompilerPath, CompilerVersion, ContractFilename, OutFilename);
                {_, _, true} ->
                    verify(State, CompilerPath, CompilerVersion, ContractFilename, OutFilename)
            end;
        Err ->
            Err
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

get_from_state(Key, State, Default) ->
    KVs = rebar_state:get(State, swagger_endpoints, []),
    proplists:get_value(Key, KVs, Default).

compiler_path(Version, Verify) ->
    case { is_valid_version(Version)
         , compiler_supports_json_aci(Version)
         , compiler_supports_validation(Version)
         , compiler_supports_fate(Version) } of
        {false, _, _, _} ->
            {error, io_lib:format("~p is an unknown compiler version", [Version])};
        {_, false, _, _} ->
            {error, io_lib:format("Compiler ~p is too old to support ACI generation", [Version])};
        {_, _, false, _} when Verify ->
            {error, io_lib:format("Compiler ~p is too old to support bytecode validation", [Version])};
        {_, _, _, false} ->
            %% TODO: add AEVM support - for now just FATE
            {error, io_lib:format("Compiler ~p is too old to support FATE", [Version])};
        _ ->
            {ok, filename:join([code:priv_dir(aesophia_cli), "bin", Version, "aesophia_cli"])}
    end.

is_valid_version(Version) ->
    {ok, KnownVersions} = file:list_dir(filename:join(code:priv_dir(aesophia_cli), "bin")),
    lists:member(Version, KnownVersions).

compiler_supports_json_aci([$v, $1 | _]) -> false;
compiler_supports_json_aci("v2.0.0") -> false;
compiler_supports_json_aci(_) -> true.

compiler_supports_validation([$v, $4, $., $0 | _]) -> false;
compiler_supports_validation([$v, $4 | _]) -> true;
compiler_supports_validation([$v, $3 | _]) -> false;
compiler_supports_validation([$v, $2 | _]) -> false;
compiler_supports_validation([$v, $1 | _]) -> false.

compiler_supports_fate([$v, $4 | _]) -> true;
compiler_supports_fate(_) -> false.

compile(State, CompilerPath, Version, InFilename, OutFilename) ->
    case {filelib:is_regular(InFilename), filelib:is_regular(OutFilename)} of
        {false, _} ->
            {error, io_lib:format("Unable to read the sophia contract: ~p", [InFilename])};
        {_, true} ->
            rebar_api:warn("Compilation will overwrite ~p", [OutFilename]),
            compile_(State, CompilerPath, Version, InFilename, OutFilename);
        _ ->
            compile_(State, CompilerPath, Version, InFilename, OutFilename)
    end.

compile_(State, CompilerPath, Version, InFilename, OutFilename) ->
    rebar_api:info("Compiling and generating the ACI of ~p using aesophia ~p saving results in ~p", [InFilename, Version, OutFilename]),
    CompilationOutput = os:cmd(format_compilation_command(CompilerPath, Version, InFilename)),
    ACIOutput = os:cmd(format_aci_command(CompilerPath, Version, InFilename)),
    Bytecode = postprocess_compilation(CompilationOutput, Version),
    ACI = postprocess_aci(ACIOutput, Version),
    file:write_file(OutFilename, jsx:encode(#{<<"bytecode">> => Bytecode, <<"aci">> => ACI})),
    rebar_api:info("Compilation successfull", []),
    {ok, State}.

verify(State, CompilerPath, Version, InFilename, OutFilename) ->
    case {filelib:is_regular(InFilename), file:read_file(OutFilename)} of
        {false, _} ->
            {error, io_lib:format("Unable to read the sophia contract: ~p", [InFilename])};
        {_, {error, _}} ->
            {error, io_lib:format("Unable to read compilation result: ~p", [OutFilename])};
        {_, {ok, Output}} ->
             case try
                  #{ <<"bytecode">> := B
                   , <<"aci">> := JText} = jsx:decode(Output, [{return_maps,false},{labels,binary}]),
                  aeaci_aci:from_string(JText, #{backend => fate}),
                  <<"cb_", _/binary>> = B,
                  {ok, B}
                catch _:_ ->
                    {error, "Invalid compilation results. Do not trust the contract!"}
                end of
                    {ok, Bytecode} ->
                        verify_(State, CompilerPath, Version, InFilename, Bytecode);
                    {error, _} = Err ->
                        Err
            end
    end.

verify_(State, CompilerPath, Version, InFilename, Bytecode) ->
    try
        <<"Validation successful", _/binary>> =
            os:cmd(format_validation_command(CompilerPath, Version, InFilename, Bytecode)),
        rebar_api:info("Validation successfull", []),
        {ok, State}
    catch
        _:_ ->
            {error, "Invalid compilation results. Do not trust the contract!"}
    end.

format_compilation_command(CompilerPath, [$v, $4 | _], InFilename) ->
    io_lib:format("~p ~p", [CompilerPath, InFilename]).

postprocess_compilation(CompilerOutput, [$v, $4 | _]) ->
    try
        <<"Bytecode:\n", Bytecode/binary>> = CompilerOutput,
        string:strip(binary_to_list(Bytecode))
    catch E:R ->
        rebar_api:abort("Compilation failed ~p ~p", [E, R])
    end.

format_aci_command(CompilerPath, _Version, InFilename) ->
    io_lib:format("~p --create_json_aci ~p", [CompilerPath, InFilename]).

postprocess_aci(CompilerOutput, [$v, $4 | _]) ->
    try
        JText = list_to_binary(CompilerOutput),
        aeaci_aci:from_string(JText, #{backend => fate}),
        JText
    catch E:R:S ->
        rebar_api:format("ACI generation failed ~p ~p ~p", [E, R, S])
    end.

format_validation_command(CompilerPath, _Version, InFilename, Bytecode) ->
    io_lib:format("~p ~p --validate \"~p\"", [CompilerPath, InFilename, Bytecode]).
