-module(eos_dictobj).
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



