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
    ],
    C = [{a,alpha},{b,beta}],
    D = [{a , alpha },{b , beta }],
    C = D,
    E = eos:set_slot(C,c , gamma),
    gamma = eos:get_slot(E,c),
    3 = eos:invoke(E,length,[]),
    F = [1,2,3,4,5],
    [2,4,6,8,10] = eos:invoke(F,map,[fun (X) -> 
        X * 2  end]),
    [2,4,6,8,10] = eos:invoke(F,map,[fun (X) -> 
        X * 2  end]),
    [2,4,6,8,10] = eos:invoke(F,map,[fun (X) -> 
        X * 2  end]),
    put('$C' , 0),
    eos:invoke(F,foreach,[fun (X) -> 
        put('$C' , get('$C') + X ) end]),
    15 = get('$C').
