- module('test_imperative').
- compile(export_all).


test() ->
    put('$a' , 100),
    put('$b' , 200),
    100 = get('$a'),
    200 = get('$b'),
    300 = get('$a') + get('$b') ,
    put('$a' , get('$a') * 2 ),
    put('$b' , get('$b') * 2 ),
    600 = get('$a') + get('$b') ,
    put('$a' , 0),
    eos_loop:while( fun()->io:format(eosstd:fmt("No.~s\n",[eosstd:to_str(get('$a'))])),
    put('$a' , get('$a') + 1 ) end,fun()->get('$a') < 10  end ),
    put('$a' , 0),
    {ok,4} = eos_loop:while( fun()->
    (fun()->EOSSYS@t_7 = (get('$a') rem 2 ) == 0 ,
    if
        EOSSYS@t_7 ->
            eos_loop:continue();
        true -> (fun()->EOSSYS@t_8 = get('$a') >= 5 ,
        if
            EOSSYS@t_8 ->
                eos_loop:break({ok,get('$a')});
            true -> []
        end end)()
    end end)(),
    put('$a' , get('$a') + 1 ) end,fun()->get('$a') < 10  end ).
