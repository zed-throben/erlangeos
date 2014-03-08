- module('test1').
- compile(export_all).

-spec f1() -> myatom.
f1() ->
    myatom.

-spec f2() -> 123.
f2() ->
    123.

-spec f3(false) -> undefined.
f3(false) ->
    undefined.

-spec f3(integer()) -> myatom|123.
f3(N) ->
    case N rem 2  of
        0  -> 
            123;
        1  -> 
            myatom
        
    end.

-spec f4(integer()) -> myatom|123.
f4(N) ->
    case N rem 2  of
        0  -> 
            f1();
        1  -> 
            f2()
        
    end.
