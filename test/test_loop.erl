- module('test_loop').
- compile(export_all).

test() ->
    A = eos_loop:times(fun () -> 
        eosstd:puts("*") end,5),
    B = eos_loop:times(fun (N) -> 
        eosstd:puts(eosstd:fmt("* ~s",[eosstd:to_str(N)])),
        
        (fun()->EOSSYS@t_4 = N == 3 ,
        if
            EOSSYS@t_4 ->
                eos_loop:break(uho);
            true -> []
        end end)() end,5),
    eosstd:puts(eosstd:fmt("B = ~s",[eosstd:to_str(B)])),
    put('$cnt' , 0),
    C = eos_loop:while( fun()->eosstd:puts(eosstd:fmt("count = ~s",[eosstd:to_str(get('$cnt'))])),
    put('$cnt' , get('$cnt') + 1 ),
    
    (fun()->EOSSYS@t_5 = get('$cnt') > 5 ,
    if
        EOSSYS@t_5 ->
            eos_loop:break(iiotoko);
        true -> []
    end end)() end,fun()->true end ),
    eosstd:puts(eosstd:fmt("C = ~s",[eosstd:to_str(C)])).
