-module(eos@list).
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

get_slot(Obj,Key)->
    case proplists:lookup(Key,Obj) of
        {Key,Value} -> Value;
        _ -> undefined
    end.

set_slot(Obj,Key,Value)->
	case get_slot(Obj,Key) of
		undefined -> [{Key,Value}|Obj];
%		_ -> [ X || A <- Obj,
%					X = case A of
%							{Key,_} -> {Key,Value};
%							_ -> A
%						end
		_ -> [	case X of
					{Key,_} -> {Key,Value};
					_ -> X
				end
				||
				X <- Obj
			]
	end.
