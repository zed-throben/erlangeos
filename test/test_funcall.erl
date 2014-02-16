- module('test_funcall').
- compile(export_all).


test() ->
    [0,1,2,3] = lists:reverse([3,2,1,0]),
    [0,1,2,3] = lists:reverse([3,2,1,0]),
    [0,1,2,3] = lists:reverse(lists:flatten([3,[2,1],0])),
    [0,1,2,3] = lists:reverse(lists:flatten([3,[2,1],0])),
    [0,1,2,3] = lists:reverse(lists:flatten([3,[2,1],0])),
    [0,1,2,3] = lists:reverse(lists:flatten([3,[2,1],0])),
    [0,1,2,3] = lists:reverse(lists:flatten([3,[2,1],0])),
    [0,1,2,3] = lists:reverse(lists:flatten([3,[2,1],0])),
    [0,1,2,3] = lists:reverse(lists:flatten([3,[2,1],0])),
    [100,101,102,103] = lists:map(fun (X) -> 
        X + 100  end,[0,1,2,3]),
    [1,2,3,4] = lists:map(fun (X) -> 
        X + 1  end,[0,1,2,3]),
    [1,2,3,4] = lists:map(fun (X) -> 
        Y = 1,
        X + Y  end,[0,1,2,3]),
    [1,2,3,4] = lists:map(fun (X) -> 
        Y = 1,
        X + Y  end,[0,1,2,3]),
    [4,3,2,1] = lists:reverse(lists:map(fun (X) -> 
        X + 1  end,[0,1,2,3])),
    [13,12,11,10] = lists:reverse(lists:map(fun (X) -> 
        X + 10  end,[0,1,2,3])),
    [13,12,11,10] = lists:reverse(lists:map(fun (X) -> 
        X + 10  end,[0,1,2,3])),
    [6,4,2] = lists:reverse(lists:map(fun (X) -> 
        X * 2  end,[1,2,3])),
    [6,4,2] = lists:reverse(lists:map(fun (X) -> 
        X * 2  end,[1,2,3])),
    [6,4,2] = lists:reverse(lists:map(fun (X) -> 
        X * 2  end,[1,2,3])),
    [6,4,2] = lists:reverse(lists:map(fun (X) -> 
        X * 2  end,[1,2,3])),
    [6,4,2] = lists:reverse(lists:map(fun (X) -> 
        X * 2  end,[1,2,3])),
    [2,4,6] = lists:map(fun (X) -> 
        X * 2  end,[1,2,3]).
