- module('test_binary').
- compile(export_all).


test() ->
    <<1,2,3>> = <<<<X>> ||
        X <- lists:seq(1,3) 
    >>,
    <<1,2,3>> = <<<<X>> ||
        X <- [1,2,3] 
    >>,
    <<1,2,3>> = << <<X>> || X<-lists:seq(1,3) >>.
