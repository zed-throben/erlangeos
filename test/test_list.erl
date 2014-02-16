- module('test_list').
- compile(export_all).


test() ->
    [1,2,3,4] = [1,2,3,4],
    [1,2,3,4] = [1,2,3,4],
    [1,2,3,4] = [1,2,3,4],
    3 = eos:invoke_list([a,b,c],length,[]),
    [A|B] = [1,2,3],
    A = 1,
    B = [2,3],
    [a,b,c,d] = [a,b,c,d|[]] = [a,b|[c,d]] = [a,b|[c|[d]]] = [a|[b|[c|[d]]]] = [a|[b|[c|[d|[]]]]],
    Weather = [{toronto,rain},{montreal,storms},{london,fog},{paris,sun},{boston,fog},{vancouver,snow}],
    [london,boston] = [X ||
        {X,fog} <- Weather 
    ].
