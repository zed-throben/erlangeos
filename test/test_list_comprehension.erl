- module('test_list_comprehension').
- compile(export_all).


pyth(N) ->
    [{A,B,C} ||
        A <- lists:seq(1,N) ,B <- lists:seq(1,N) ,C <- lists:seq(1,N) ,A + B + C =< N   ,A * A + B * B == C * C     
    ].


test() ->
    [{a,1},{a,2},{a,3}] = [{a,X} ||
        X <- [1,2,3] 
    ],
    [{1,10},{1,20},{2,10},{2,20}] = [{X,Y} ||
        X <- [1,2] ,Y <- [10,20] 
    ],
    [a,b,c] = [case X of
        0  -> 
            a;
        1  -> 
            b;
        2  -> 
            c
        
    end ||
        X <- [0,1,2] 
    ],
    [a,4,b,5,6] = [X ||
        X <- [1,2,a,3,4,b,5,6] ,X > 3 
    ],
    [a,4,b,5,6] = [X ||
        X <- [1,2,a,3,4,b,5,6] ,X > 3 
    ],
    [{3,4,5},{4,3,5}] = pyth(12).
