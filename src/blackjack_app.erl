%%%-------------------------------------------------------------------
%% @doc blackjack public API
%% @end
%%%-------------------------------------------------------------------

-module(blackjack_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    blackjack_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
