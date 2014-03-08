-module(erleos_emit_term).
-compile(export_all).
-include("erleos_parser.hrl").
-include("eos_modobj.hrl").

init_module_object()->
    [].

%


%

emit_kv({Key,Value})->
    { e(Key),e(Value) }.

emit_kvlist_([],Acc)->
    lists:reverse(Acc);
emit_kvlist_([H|T],Acc)->
    emit_kvlist_(T,[emit_kv(H)|Acc]).

emit_kvlist(Src)->
    emit_kvlist_(Src,[]).

%


emit_list_([],Acc)->
    lists:reverse(Acc);
emit_list_([H|T],Acc)->
    emit_list_(T,[e(H)|Acc]).

emit_list(Src)->
    emit_list_(Src,[]).


%

e( ?t('-',Value) )->
    - e(Value);

e( ?t('+',Value) )->
    e(Value);

e( ?t(proplist,KeyValues) )->
    emit_kvlist(KeyValues);

e( ?t(list_sequence,{A,B}) )->
    lists:seq(A,B);

e( ?t(list_sequence2,{A,B}) )->
    lists:seq(A,B-1);

e( ?t(list,List) )->
    emit_list(List);

e( ?t(binary_sequence,[A,B]) )->
    << <<X>> || X<-lists:seq(A,B) >>;

e( ?t(binary_sequence2,[A,B]) )->
    << <<X>> || X<-lists:seq(A,B-1) >>;

e( ?t(binary,List) )->
    list_to_binary( emit_list(List) );

e( ?t(tuple,List) )->
    list_to_tuple( emit_list(List) );

e( ?t(stringlist,Str) ) ->
    eosstd:to_list(Str);

e( ?t(binarystring,Str) ) ->
    eosstd:to_bin(Str);

e( ?t(char,[Ch]) ) ->
    e(Ch);

e( ?t(quotedsymbol,X) )->
    eosstd:to_atom(X);

e( ?t(symbol,X) )->
    eosstd:to_atom(X);

e( ?t(number,X) )->
    X;

e( ?t(n_int,{N,X}) )->
    list_to_integer(X,N);

e( ?t(e_int,{X,N}) )->
    list_to_float( eosstd:fmt("~we~w",[X,N]) );

%e( List ) when is_list(List) ->
%    io:format("**list ~p\n",[List]),
%    emit_list(List);

e([H])->
    e(H);

e(X)->
    X.

%

emit([Src])->
    e(Src).

%

init()->
    [].

init(F)->
    [].

%


exec_emit_block(_HdSrc,Src)->
    emit(Src).


%

