- module('test_case').
- compile(export_all).


f(X) ->
    {ok,okok}.


test(URL) ->
    {ok,okok} = case f(URL) of
        { ok ,  _ } = Res  -> 
            Res;
        error  -> 
            timer:sleep(2500),
            case f(URL) of
                { ok ,  _ } = Res  -> 
                    Res;
                error  -> 
                    timer:sleep(10000),
                    f(URL)
                
            end
        
    end.


test() ->
    test(0).
