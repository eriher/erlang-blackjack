-module(blackjack_dealer).
-behaviour(gen_server).
-define(NAME, blackjack_dealer).

%% API
-export([stop/0, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([startGame/0,drawCards/1,handValue/1]).
-record(state, {deck,dealer,player,record}).

stop() ->
    gen_server:call(?NAME, stop).

start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

startGame()->
    gen_server:call(?NAME, (start)).

drawCards(N)->
    gen_server:call(?NAME, {drawCards, N}).

init(_Args) ->
    {ok, #state{deck=[],dealer=[],player=[],record={0,0}}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(start, _From, #state{record=Record}) ->
    {Player,Deck} = drawTwoCards(shuffleDeck(generateDeck())),
    {Dealer, NewDeck} = drawTwoCards(Deck),
    {reply, {ready, Player, tl(Dealer), Record} , #state{deck=NewDeck,dealer=Dealer,player=Player,record=Record}};

%player wants to hold current cards, play out game for dealer
handle_call({drawCards, 0}, _From, #state{deck=Deck,dealer=Dealer,player=Player,record={Win,Loss}}) ->
    {Result, DealerHand, NewDeck} = playDealer(handValue(Player),handValue(Dealer),Dealer,Deck),
    case Result of
        win -> {NewWin,NewLoss} = {Win+1,Loss};
        _ -> {NewWin,NewLoss} = {Win,Loss+1}
    end,
    {reply, {Result, Player, lists:reverse(DealerHand)}, #state{deck=NewDeck,dealer=DealerHand,player=Player,record={NewWin,NewLoss}}};
    
%draw 1 card for player, if new total is above 21 it is a bust and player loses, otherwise continue playing    
handle_call({drawCards, 1}, _From, #state{deck=Deck,dealer=Dealer,player=Player,record={Win,Loss}}) ->
    {Card,NewDeck} = drawCard(Deck),
    NewPlayer = [Card|Player],
    case isBust(NewPlayer) of
        true -> {reply, {loss,NewPlayer,Dealer}, #state{deck=NewDeck,dealer=Dealer,player=NewPlayer,record={Win,Loss+1}}};
        _ -> {reply, {ready, NewPlayer,tl(Dealer)}, #state{deck=NewDeck,dealer=Dealer,player=NewPlayer,record={Win,Loss}}}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
generateDeck() -> 
    [{S,C} || S <- [spades, clubs, hearts, diamonds],  C <- lists:seq(2,10)++[jack,queen,king,ace]].

shuffleDeck(Deck) ->
    [Y||{_,Y} <- lists:sort([ {rand:uniform(), N} || N <- Deck])].

drawCard(Deck) ->
    {hd(Deck), tl(Deck)}.

drawTwoCards([C1|[C2|C3]]) ->
    {[C1,C2],C3}.

handValue(Hand)->handValue(Hand,0,0).
handValue([],V1,V2) when V2 > 21 -> V1;
handValue([],V1,V2) -> V2;
handValue([{Suit,Rank}|H],V1,V2) when Rank == ace -> handValue(H,V1+1,V2+11);
handValue([{Suit,Rank}|H],V1,V2) -> handValue(H,V1+cardValue(Rank),V2+cardValue(Rank)).


isBust(Hand) -> 
    handValue(Hand) > 21.

cardValue(king) -> 10;
cardValue(queen) -> 10;
cardValue(jack) -> 10;
cardValue(C) -> C.

playDealer(PlayerScore, DealerScore, Hand, Deck) when (DealerScore > 21) ->
    {win,Hand,Deck};
playDealer(PlayerScore, DealerScore, Hand, Deck) when (DealerScore >= PlayerScore) ->
    {loss,Hand,Deck};
playDealer(PlayerScore,DealerScore, Hand,Deck) ->
    NewHand = [hd(Deck)|Hand],
    playDealer(PlayerScore,handValue(NewHand),NewHand,tl(Deck)).
    
    
