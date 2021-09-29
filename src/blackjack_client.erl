-module(blackjack_client).
-behaviour(gen_statem).
-define(NAME, blackjack_client).

-export([stop/0, start_link/0]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).
-export([initializing/3, playing/3]).

stop() ->
    gen_statem:stop(?MODULE).

start_link() ->
    gen_statem:start_link({local, ?NAME}, ?MODULE, [], []).

%%Private API
read_input(Question) ->
    case io:fread(Question, "~s") of
        {ok, [C]} when (C == "y") or (C == "Y") -> 1;
        {ok, [C]} when (C == "n") or (C == "N") -> 0;
        _ -> io:format("Answer with (y/n).~n"), read_input(Question)
    end.

another_round(1) -> {next_state, initializing, {}};
another_round(0) -> {stop, normal}.

init(_Args) ->
    io:format("init blackjack client.~n"),
    {ok, initializing, []}.

%% state_functions | handle_event_function | [_, state_enter].
callback_mode() ->
    [state_functions, state_enter].

initializing(enter, _EventContent, Data) ->
    %% force trigger the state handler
    gen_statem:cast(?NAME, {start}),
    {next_state, initializing, Data};

initializing(_EventType, _EventContent, _Data) ->
    case blackjack_dealer:startGame() of
        {ready, Player,Dealer,{Wins,Losses}} ->
            io:format("Round: ~w, Wins: ~w, Losses: ~w~n",[Wins+Losses+1,Wins,Losses]),
            printHand(Dealer,"Dealer"),
            printHand(Player,"Player"),
            {next_state, playing, {Player,Dealer}};
        Other ->
            io:format("Received unexpected response ~w from server, terminating~n...", [Other]),
            {stop, {unknown, Other}}
    end.

%% In-game state
playing(enter, _OldState, Data) ->
    %% force trigger the state handler
    gen_statem:cast(?NAME, {}),
    {next_state, playing, Data};
playing(_EventType, _OldState, Data) ->
    Cards = read_input("Draw card? (y/n) "),
    {State,Player,Dealer} = blackjack_dealer:drawCards(Cards),
    if Cards > 0 -> 
        {Suit,Rank} = hd(Player),
        io:format("Drew ~w of ~w, ",[Rank,Suit]);
        true -> 
            io:format("Holding~n")
    end,
    case State of
        ready ->
            io:format("score: ~w~n", [blackjack_dealer:handValue(Player)]),
            {repeat_state, {Player, Dealer}};
        win ->
            printFinalHand(Dealer),
            timer:sleep(1000),
            io:format("BUST!, final score: ~w~n", [blackjack_dealer:handValue(Dealer)]),
            io:format("YOU WON!!~n"),
            another_round(read_input("Play again? (y/n) "));
        loss ->
            case blackjack_dealer:handValue(Player) > 21 of
                true ->
                        io:format("Bust! You LOST! ~n"),
                        printHand(Player,"Full hand"),
                        another_round(read_input("Play again? (y/n) "));
                _ ->
                        printFinalHand(Dealer),
                        timer:sleep(1000),
                        io:format("score: ~w~n", [blackjack_dealer:handValue(Dealer)]),
                        io:format("You LOST!~n"),
                        another_round(read_input("Play again? (y/n) "))
            end;
        _ -> {keep_state, Data}
    end.

handle_event(enter, _OldState, _State, _Data) ->
    keep_state_and_data;

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

printHand(Hand,Owner) -> 
    io:format("~s:", [Owner]),
    lists:foreach(fun({Suit,Rank})->io:format(" ~w of ~w,",[Rank,Suit]) end, Hand),
    io:format(" score: ~w~n", [blackjack_dealer:handValue(Hand)]).

printFinalHand([{S,R}|[{S2,R2}|Hand]]) ->
    io:format("Dealer has ~w of ~w, ",[R,S]),
    io:format("and reveals "),
    timer:sleep(2000),
    io:format("~w of ~w, ",[R2,S2]),
    lists:foreach(fun({Suit,Rank})->timer:sleep(2000),io:format("draws ~w of ~w, ",[Rank,Suit]) end, (Hand)).