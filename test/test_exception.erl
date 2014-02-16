- module('test_exception').
- compile(export_all).


f(N) ->
    N * 2 .


test() ->
    aaa = try
        AA = 10,
        BB = AA - f(5) ,
        CC = AA / BB 
    catch
        error : Reason  -> 
            eosstd:puts(eosstd:fmt("**error ~p",[Reason])),
            aaa;
        throw : Reason  -> 
            eosstd:puts(eosstd:fmt("**throw ~p",[Reason])),
            bbb
        
    end.
