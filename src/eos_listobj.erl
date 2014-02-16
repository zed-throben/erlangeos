-module(eos_listobj).
-compile(export_all).

length(List)->
	erlang:length(List).

to_bin(List)->
	erlang:list_to_binary(List).

to_list(List)->
	List.

to_atom(List)->
	erlang:list_to_atom(List).

%

reverse(List)->
	lists:reverse(List).

foreach(List,F)->
	lists:foreach(F,List).

map(List,F)->
	lists:map(F,List).

foldl(List,F,Acc)->
	lists:foldl(F,Acc,List).

foldr(List,F,Acc)->
	lists:foldr(F,Acc,List).



