- module('test_record').
- compile(export_all).
- record(rec,{a,b,c}).

test() ->
    A = #rec{a  = alpha },
    B = #rec{a  = alpha ,b  = beta ,c  = gamma },
    C = #rec{a  = aaa ,b  = bbb ,c  = ccc },
    D = (A#rec.a),
    E = #rec{a  = fun (X) -> 
        io:format("hello ~s\n",[X]) end },
    (E#rec.a)("record").
