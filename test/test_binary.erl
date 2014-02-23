- module('test_binary').
- compile(export_all).


truncate(X) when X < 0  ->
    0;

truncate(X) when X > 255  ->
    255;

truncate(X) ->
    X.


test() ->
    <<1,2,3>> = <<<<X>> ||
        X <- lists:seq(1,3) 
    >>,
    <<1,2,3>> = <<<<X>> ||
        X <- [1,2,3] 
    >>,
    <<1,2,3>> = << <<X>> || X<-lists:seq(1,3) >>,
    <<R:8,G:8,B:8>> = <<50,100,150>>,
    <<255>> = <<(truncate(1000))>>,
    <<0>> = <<(truncate(-1000))>>.
