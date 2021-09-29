%%%-------------------------------------------------------------------
%% @doc blackjack top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(blackjack_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    io:format("[supervisor] Booting up...~n"),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
        #{id => dealer,
            start => {blackjack_dealer, start_link, []},
            restart => transient, type => worker,
            modules => [blackjack_dealer]},
            #{id => blackjack_client, start => {blackjack_client, start_link, []}, 
            restart => transient, type => worker,
            modules => [blackjack_dealer]}
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
