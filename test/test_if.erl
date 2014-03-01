- module('test_if').
- compile(export_all).

test() ->
    30 = test_if(fun () -> 
        0 end),
    b = test_if(fun () -> 
        1 end),
    200 = test_if(fun () -> 
        2 end),
    d = test_if(fun () -> 
        3 end).

test_if(F) ->
    
    (fun()->EOSSYS@t_9 = F() == 0 ,
    if
        EOSSYS@t_9 ->
            io:format("case 1\n",[]),
            AA = 10,
            BB = 20,
            AA + BB ;
        true -> (fun()->EOSSYS@t_10 = F() == 1 ,
        if
            EOSSYS@t_10 ->
                io:format("case 2\n",[]),
                b;
            true -> (fun()->EOSSYS@t_11 = F() == 2 ,
            if
                EOSSYS@t_11 ->
                    io:format("case 3\n",[]),
                    CC = 100,
                    CC * 2 ;
                true -> io:format("case 4\n",[]),
                d
            end end)()
        end end)()
    end end)().
