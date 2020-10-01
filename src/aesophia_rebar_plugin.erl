-module(aesophia_rebar_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  application:ensure_started(jsx),
  {ok, State1} = aesophia_rebar_plugin_prv:init(State),
  {ok, State1}.
