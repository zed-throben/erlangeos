-module(eosstd).

-compile(export_all).
-include("eos.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.

to_bin(X) when is_binary(X) -> X;
to_bin(X) when is_list(X) -> list_to_binary(X).

to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_binary(X) -> binary_to_atom(X,utf8);
to_atom(X) when is_list(X) -> list_to_atom(X).

to_str(X) when is_binary(X) ->
	fmt("~s",[X]);
to_str(X) when is_list(X) ->
	fmt("~s",[X]);
to_str(X)->
	fmt("~p",[X]).

fmt(Fmt,Param)->
	lists:flatten( io_lib:format(Fmt,Param)).
fmt(Fmt)->
    to_list(Fmt).

fmt_bin(Fmt,Param)->
    list_to_binary( fmt(Fmt,Param) ). 
fmt_bin(Fmt)->
    list_to_binary( fmt(Fmt) ). 

fmt_atom(Fmt,Param)->
    list_to_atom( fmt(Fmt,Param) ). 
fmt_atom(Fmt)->
    list_to_atom( fmt(Fmt) ). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sconcat([],Acc)->Acc;%%lists:reverse(Acc);
sconcat([Hd|Tl],Acc) when is_list(Hd)-> sconcat(Tl,Acc++Hd);
sconcat([Hd|Tl],Acc) when is_binary(Hd)-> sconcat(Tl,Acc++binary_to_list(Hd));
sconcat([Hd|Tl],Acc) when is_integer(Hd) -> sconcat(Tl,Acc++[Hd]).

sconcat(X)->
	sconcat(X,[]).	

lsformat(Fmt,Params)->
	sconcat( io_lib:format(Fmt,Params) ).
sformat(Fmt,Params)->
	list_to_binary( lsformat(Fmt,Params)  ).

rec(M) ->
    G = fun (F) -> M(fun(A) -> (F(F))(A) end) end,
    G(G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%load_nif(Filename,LoadInfo)->
%	Dir = ["./","priv/","../priv/"],
%	F = fun(F)->
%			fun	([])->false;
%				([H|T])->	Path = lsformat("~s~s",[H,Filename]),
%						io:format("path + ~s~n",[Path]),
%						R  = erlang:load_nif( Path,LoadInfo ),
%						io:format("res = ~p~n",[R]),
%						if	R == true -> true;
%							true -> F(T)
%						end
%			end
%		end,
%	(rec(F))(Dir).


print(X)->
    io:format(X).

puts(X)->
    io:format("~s\n",[X]).

map(F,?DICTIONARY=Obj)->
    dict:map(F,Obj);

map(F,Obj) when is_list(Obj)->
    lists:map(F,Obj).

foreach(F,?DICTIONARY=Obj)->
    eos_dictobj:foreach(Obj,F);

foreach(F,Obj) when is_list(Obj)->
    eos_listobj:foreach(Obj,F).

