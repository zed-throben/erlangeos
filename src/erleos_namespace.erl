-module(erleos_namespace).
-compile(export_all).

standard_namespace()->
	[
		{map,<<"eosstd:map">>},
		{foreach,<<"eosstd:foreach">>},
		{fold,<<"eosstd:fold">>},
		{length,<<"eosstd:length">>},
		{print,<<"eosstd:puts">>},
		{puts,<<"eosstd:puts">>}
	].

init()->
	Dict = dict:from_list().

convert(Dict,Key)->
	try
		dict:fetch(Key,Dict)
	catch
		_:_ -> Key
	end.
	

