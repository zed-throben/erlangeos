- module('test_block').
- compile(export_all).

test() ->
    A = fun (X) -> 
        Y = X + 1 ,
        Z = Y + 2  end,
    B = fun (X) -> 
        Y = X + 1 ,
        Z = Y + 2  end,
    C = fun (X) -> 
        Y = X + 1 ,
        Z = Y + 2  end,
    D = fun (X) -> 
        Y = X + 1 ,
        Z = Y + 2  end,
    E = [fun () -> 
        A = 1,
        B = 2 end,fun () -> 
        A = 3,
        B = 4 end,fun () -> 
        A = 5,
        B = 6 end,fun () -> 
        A = 7,
        B = 8 end,9,10,11,12],
    F = begin
        io:format("alpha\n"),
        io:format("beta\n"),
        io:format("gamma\n")
    end,
    G = begin
        io:format("alpha\n"),
        io:format("beta\n"),
        io:format("gamma\n")
    end.
