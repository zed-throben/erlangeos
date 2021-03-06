-module(eos@dict).
-compile(export_all).

length(Dict)->
	dict:size(Dict).

to_list(Dict)->
	dict:to_list(Dict).

keys(Dict)->
	dict:fetch_keys(Dict).


%

foreach(Dict,F)->
	fold(Dict,fun(K,V,Acc)->F(K,V) end,undefined).

map(Dict,F)->
	dict:map(F,Dict).

fold(Dict,F,Acc)->
	dict:fold(F,Acc,Dict).

new(_Option,Params)->
	lists:foldl(
		fun({Key,Value},Dict)->
			dict:store(Key,Value,Dict)
		end
		,dict:new()
		,Params
	).



set_slot(Obj,Key,Value)->
	dict:store(Key,Value,Obj).

get_slot(Obj,Key) ->
    case dict:find(Key,Obj) of
        {ok,Value} -> Value;
        _ -> undefined
    end.


