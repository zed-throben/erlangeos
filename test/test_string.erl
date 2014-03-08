- module('test_string').
- compile(export_all).

test() ->
    A = <<"abc\ndef\n">>,
    B = <<"abc\ndef">>,
    io:format("A = ~p\n",[A]),
    io:format("B = ~p\n",[B]).
