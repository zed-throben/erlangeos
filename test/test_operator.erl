- module('test_operator').
- compile(export_all).

test() ->
    true = 1 == 1 ,
    false = 1 =:= 1.0 ,
    true = 1 /= 2 ,
    false = 1 /= 1 ,
    true = 1.0 =:= 1.0 ,
    false = 1 =:= 1.0 ,
    true = 1 =:= 1 ,
    true = 1.0 =:= 1.0 ,
    false = 1 =/= 1 ,
    false = 1.0 =/= 1.0 ,
    true = 1 =/= 1.0 ,
    true = 1 =< 1 ,
    true = 1 >= 1 ,
    true = 1 < 2 ,
    true = 2 > 1 ,
    false = 1 < 1 ,
    false = 1 > 1 .
