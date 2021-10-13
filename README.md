A simple erlang blackjack game
=====

-Launches a gen_server, the dealer and a gen_fsm client.

-Plans to:
*Some type of betting with user tracking
*Support multiple tables/dealers with differing betting rules
*Multiple clients

Build
-----

    $ rebar3 compile
Run
-----

    $ erl -pa _build/default/lib/blackjack/ebin -noshell -eval 'application:start(blackjack)'
