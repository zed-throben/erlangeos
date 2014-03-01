- module('test_where').
- compile(export_all).

test() ->
    testN(0).

testN(N) ->
    eosstd:puts(eosstd:fmt("test #~s",[eosstd:to_str(N)])),
    
    (fun()->EOSSYS@t_0 = test(N) /= eof ,
    if
        EOSSYS@t_0 ->
            testN(N + 1 );
        true -> []
    end end)().

test(0) ->
    ( fun(A ,B ) -> 
        30 = A + B 
    end)(10 ,20 );

test(1) ->
    ( fun(A ,B ) -> 
        X = A + B ,
        Y = 100,
        130 = X + Y 
    end)(10 ,20 );

test(2) ->
    A = 10,
    B = 
    (fun()->EOSSYS@t_1 = false,
    if
        EOSSYS@t_1 ->
            ng;
        true -> ( fun(X ,Y ) -> 
            X + Y 
        end)(300 ,400 )
    end end)(),
    eosstd:puts(eosstd:fmt("B = ~s",[eosstd:to_str(B)])),
    700 = B;

test(3) ->
    ( fun(Z ) -> 
        A = 10,
        B = 
        (fun()->EOSSYS@t_2 = false,
        if
            EOSSYS@t_2 ->
                ng;
            true -> ( fun(X ,Y ) -> 
                X + Y + Z  
            end)(300 ,400 )
        end end)(),
        1700 = B
    end)(1000 );

test(4) ->
    ( fun(Z ) -> 
        A = 10,
        B = 
        (fun()->EOSSYS@t_3 = A < 10 ,
        if
            EOSSYS@t_3 ->
                ( fun(X ,Y ) -> 
                    X + Y + Z  
                end)(100 ,200 );
            true -> ( fun(X ,Y ) -> 
                X + Y + Z  
            end)(300 ,400 )
        end end)(),
        1700 = B
    end)(1000 );

test(5) ->
    eof.
