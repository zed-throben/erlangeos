- module('test_let').
- compile(export_all).

test() ->
    testN(0).

testN(N) ->
    eosstd:puts(eosstd:fmt("test #~s",[eosstd:to_str(N)])),
    
    (fun()->EOSSYS@t_6 = test(N) /= eof ,
    if
        EOSSYS@t_6 ->
            testN(N + 1 );
        true -> []
    end end)().

test(0) ->
    ( fun(A ) -> 
        A + 10 
    end)(10 );

test(1) ->
    ( fun(A ) -> 
        ( fun(B ) -> 
            A + B 
        end)(20 )
    end)(10 );

test(2) ->
    ( fun(A ,B ) -> 
        A + B 
    end)(10 ,20 );

test(3) ->
    ( fun(A ,B ) -> 
        A + B 
    end)(10 ,20 );

test(4) ->
    ( fun(A ,{ X ,  Y } ) -> 
        A + X + Y  
    end)(10 ,{100,200} );

test(5) ->
    eof.
