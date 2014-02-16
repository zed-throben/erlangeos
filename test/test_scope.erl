- module('test_scope').
- compile(export_all).


test() ->
    ( fun(X ) -> 
        [1,2,3,4] = X
    end)(lists:map(fun (X) -> 
        Y = 1,
        X + Y  end,[0,1,2,3]) ),
    ( fun(X ,Y ) -> 
        [1,2,3,4] = X,
        [2,3,4,5] = Y
    end)(lists:map(fun (X) -> 
        Y = 1,
        X + Y  end,[0,1,2,3]) ,lists:map(fun (X) -> 
        Y = 2,
        X + Y  end,[0,1,2,3]) ).
