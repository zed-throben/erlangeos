-module(erleos_emit).
-compile(export_all).
-include("erleos_parser.hrl").
-include("eos_modobj.hrl").

init_module_object()->
    [].

to_lower(X)->
    List = eosstd:to_list(X),
    string:to_lower(List).

indent(N)->
    put(indent,get(indent)+N).

%

emit_tab(0) ->
    [];
emit_tab(1) ->
    p(" ");
emit_tab(2) ->
    p("  ");
emit_tab(3) ->
    p("   ");
emit_tab(N)->
    p("    "),
    emit_tab(N-4).

%

add_to_cur_form(S)->
    put(cur_form,
        get(cur_form) ++ S).

outstr(S)->
    %io:format("~s",[S]).
    ( get(outstr) )(S).

p(X)->
    S = eosstd:fmt("~s",[X]),
    add_to_cur_form(S),
    outstr(S).

pp(X)->
    S = eosstd:fmt("~p",[X]),
    add_to_cur_form(S),
    outstr(S).

p(Fmt,Param)->
    S = eosstd:fmt(Fmt,Param),
    add_to_cur_form(S),
    outstr(S).

cr()->
    %p("\n"),
    add_to_cur_form("\n"),
    emit_tab( get(indent)*4 ).

endform()->
    CurForm = lists:flatten( get(cur_form) ),
    %io:format(">>> ~s <<<\n",[CurForm]),
    put(forms,[CurForm|get(forms)]),
    put(cur_form,"").

%

str(X) when is_atom(X) ->
    atom_to_binary(X,utf8);
%str(X) when is_list(X) ->
%    list_to_binary(X,utf8);

str({var,X})->
    str(X);

str({symbol,X})->
    str(X);

str({quoted_symbol,X})->
    "'"++str(X)++"'";

str(X) ->
    X.

%

type( ?t(list,List) )->
    list;
type(X) when is_atom(X)->
    atom;
type(_)->
    unknown.

%

strof( ?t(record_name,X) )->
    X;
strof( ?t(var,X) )->
    X;
strof( ?t(quotedsymbol,X) )->
    eosstd:fmt("'~s'",[X]);
strof( ?t(symbol,X) )->
    X.


resolve_namespace( ?t(symbol,'while') )->
    <<"eos_loop:while">>;

resolve_namespace( ?t(symbol,'continue') )->
    <<"eos_loop:continue">>;

resolve_namespace( ?t(symbol,'break') )->
    <<"eos_loop:break">>;

resolve_namespace( ?t(recpath,{Obj,Rec,Member}) )->
    eosstd:fmt("(~s#~s.~s)",[strof(Obj),strof(Rec),strof(Member)]);

resolve_namespace( ?t(var,X) )->
    X;

resolve_namespace( ?t(quotedsymbol,X) )->
    X;

resolve_namespace( ?t(symbol,X) )->
    X.


%

emit_kv({Key,Value})->
    p("{"),
    e(Key),
    p(", "),
    e(Value),
    p("}").

emit_kvlist([])->
    true;
emit_kvlist([H])->
    emit_kv(H);
emit_kvlist([H|T])->
    emit_kv(H),
    p(","),
    emit_kvlist(T).

%

emit_keyvalue({Key,Value})->
    e(Key),
    p(" = "),
    e(Value).

emit_dictionary([])->
    true;
emit_dictionary([H])->
    emit_keyvalue(H);
emit_dictionary([H|T])->
    emit_keyvalue(H),
    p(","),
    emit_dictionary(T).

%

emit_list([])->
    true;
emit_list([H])->
    e(H);
emit_list([H|T])->
    e(H),
    p(","),
    emit_list(T).

%

emit_list2([])->
    true;
emit_list2([H])->
    p(","),
    e(H);
emit_list2([H|T])->
    p(","),
    e(H),
    emit_list(T).

%

emit_str(Str)->
    ErlSrc = erleos:translate_block(Str),
    p(ErlSrc).

emit_str_list([])->
    true;
emit_str_list([H])->
    emit_str(H);
emit_str_list([H|T])->
    emit_str(H),
    p(","),
    emit_str_list(T).

%

emit_lambda_({'fun',Arity,Param,Body} )->
    p("("),
    emit_list(Param),
    p(") -> "),
    indent(1),
    cr(),
    emit_block(Body),
    indent(-1);

emit_lambda_({'fun2',Arity,Param,Guard,Body} )->
    p("("),
    emit_list(Param),
    p(") when "),
    emit(Guard),
    p(" -> "),
    indent(1),
    cr(),
    emit_block(Body),
    indent(-1).

emit_method_({'fun',Arity,Param,Body} )->
    p("(This,Arguments) ->"),
    if  Param == [] -> [];
        true ->
            p("["),
            emit_list(Param),
            p("|RestArguments] = Arguments,")
    end,
    indent(1),
    cr(),
    emit_block(Body),
    indent(-1).

% member does not support guard clause
%emit_method_({'fun2',Arity,Param,Guard,Body} )->
%    p("(This,Arguments) when "),
%    emit(Guard),
%    p(" -> "),
%    p("["),
%    emit_list(Param),
%    p("|RestArguments] = Arguments,"),
%    emit_block(Body).


%

emit_lambda([])->
    [];

emit_lambda([H])->
    emit_lambda_(H),
    p(" end");

emit_lambda([H|T])->
    emit_lambda_(H),
    p(";"),
    cr(),
    emit_lambda(T).

emit_method([H])->
    emit_method_(H),
    p(" end").

%

emit_case({Pattern,Body})->
    emit(Pattern),
    p(" -> "),
    indent(1),
    cr(),
    emit_block(Body),
    indent(-1).

%

emit_cases([])->
    [];

emit_cases([H])->
    emit_case(H),
    cr();

emit_cases([H|T])->
    emit_case(H),
    p(";"),
    cr(),
    emit_cases(T).

emit_char(Ch)->
    if
        ($a =< Ch) and (Ch =< $z) -> p("$~c",[Ch]);
        ($A =< Ch) and (Ch =< $Z) -> p("$~c",[Ch]);
        ($0 =< Ch) and (Ch =< $9) -> p("$~c",[Ch]);
        true -> p("~w",[Ch])
    end.

emit_chars([])->
    [];
emit_chars([H])->
    emit_char(H);
emit_chars([H|T])->
    emit_char(H),
    p(","),
    emit_chars(T).

%
emit_if([])->
    [];

emit_if([ELSE])->
    emit_block(ELSE);

emit_if([IFCOND,THEN | Rest])->
    %io:format("IFCOND = ~p\n",[IFCOND]),
    %io:format("THEN = ~p\n",[THEN]),
    p("(fun()->"),

        Cond = erleos_gensym:alloc("EOSSYS"),
        p("~s = ",[Cond]),
        e(IFCOND),
        p(","),
        cr(),

        p("if"),
        indent(1),
        cr(),
        p("~s ->",[Cond]),
        indent(1),
        cr(),

        emit_block(THEN),
        p(";"),
        indent(-1),
        cr(),

        p("true -> "),
        if Rest == [] -> p("[]");
            true -> emit_if(Rest)
        end,

        indent(-1),
        cr(),
        p("end"),

    p(" end)()").


%

e( ?t('-',Value) )->
    p("-"),
    e(Value);

e( ?t('+',Value) )->
    p("+"),
    e(Value);

e( ?t('funref',Funsym) )->
    p("fun "),
    %io:format("funref = ~p\n",[Funsym]),
    case Funsym of
        ?token(_,symbol,A) ->
            e(Funsym);
        ?token(_,objpath,[A,B]) ->
            e(A),
            p(":"),
            e(B)
    end;

e( ?t('fun',Definitions) )->
    p("fun "),
    emit_lambda(Definitions);

e( ?t('method',Definitions) )->
    p("fun "),
    emit_method(Definitions);


e( ?t('while',{Expr,Block}) )->
    %io:format("@@expr = ~p\n",[Expr]),
    %io:format("@@block = ~p\n",[Block]),
    p("eos_loop:while( fun()->"),
    emit_block(Block),
    p(" end,fun()->"),
    e(Expr),
    p(" end )");

e( ?t('monad',{Expr,Block}) )->
    %io:format("@@expr = ~p\n",[Expr]),
    %io:format("@@block = ~p\n",[Block]),
    p("( fun(EOS@Monad)-> eos:using(EOS@Monad,fun()->"),
    emit_block(Block),
    p(" end) end)( eos:monad("),
    emit_block(Expr),
    p("))");

e( ?t('bind',{Name,Expr,Block}) )->
    p("eos:invoke(EOS@Monad,bind,[ fun("),
    e(Name),
    p(")->"),
    emit_block(Block),
    p(" end,"),
    emit_block(Expr),
    p("])");


%

%e( ?t(dictionary,KeyValues) )->
%    p("["),
%    emit_kvlist(KeyValues),
%    p("]");

e( ?t(proplist,KeyValues) )->
    p("["),
    emit_kvlist(KeyValues),
    p("]");

e( ?t(newobj,{TypeName,Param,KeyValues}) )->
    p("eos:new("),
    e(TypeName),
    p(",["),
    emit_list(Param),
    p("],["),
    emit_kvlist(KeyValues),
    p("])");

e( ?t(paren,List) )->
    %io:format("****emit paren = ~p\n",[List]),
    p("("),
    if is_list(List)->emit(List);
        true->e(List)
    end,
    p(")");

e( ?t(list_comprehension,{A,B}) )->
    p("["),
    e(A),
    p(" ||"),
    indent(1),
    cr(),
    emit_list(B),
    indent(-1),
    cr(),
    p("]");

e( ?t(list_sequence,{A,B}) )->
    p("lists:seq("),
    e(A),
    p(","),
    e(B),
    p(")");

e( ?t(list_sequence2,{A,B}) )->
    p("lists:seq("),
    e(A),
    p(","),
    e(B),
    p("-1)");

e( ?t(list,List) )->
    p("["),
    emit_list(List),
    p("]");

e( ?t(list_cons,{List,CDR}) )->
    p("["),
    emit_list(List),
    p("|"),
    e(CDR),
    p("]");

e( ?t(binary_comprehension,[H|T]) )->
    p("<<"),
    e(H),
    p(" ||"),
    indent(1),
    cr(),
    emit_list(T),
    indent(-1),
    cr(),
    p(">>");

e( ?t(binary_sequence,[A,B]) )->
    p("<< <<X>> || X<-lists:seq("),
    e(A),
    p(","),
    e(B),
    p(") >>");

e( ?t(binary_sequence2,[A,B]) )->
    p("<< <<X>> || X<-lists:seq("),
    e(A),
    p(","),
    e(B),
    p("-1) >>");

e( ?t(binary,List) )->
    p("<<"),
    emit_list(List),
    p(">>");

e( ?t(tuple,List) )->
    p("{"),
    emit_list(List),
    p("}");

e( ?token(LnRw,fmt_stringlist,String)=T ) ->
    try
        {Fmt,Params} = erleos_formatted_string:convert(String),
        p("eosstd:fmt(\"~s\",[",[Fmt]),
        emit_str_list(Params),
        p("])")
    catch
        throw:X ->
            erleos:compile_error( compile_error,LnRw,eosstd:fmt("invalid formatted string: ~p",[T]) )
    end;



e( ?token(LnRw,fmt_binarystring,String)=T ) ->
    try
        {Fmt,Params} = erleos_formatted_string:convert(String),
        p("list_to_binary( eosstd:fmt(\"~s\",[",[Fmt]),
        emit_str_list(Params),
        p("]) )")
    catch
        throw:X ->
            erleos:compile_error( compile_error,LnRw,eosstd:fmt("invalid formatted binary string: ~p",[T]) )
    end;

e( ?t(erlang_direct,Str) ) ->
    p("~s",[Str]);

e( ?t(stringlist,Str) ) ->
    p("\"~s\"",[Str]);

e( ?t(binarystring,Str) ) ->
    p("<<\"~s\">>",[Str]);

e( ?t(char,Chrs) ) ->
    emit_chars(Chrs);

% module:function
e( ?t(path,[Module,Function]) )->
    e(Module),
    p(":"),
    e(Function);

% try

e( ?t('try',{TryBlock,Cases}) ) ->
    p("try"),
    indent(1),
    cr(),
    emit_block(TryBlock),
    indent(-1),
    cr(),
    p("catch"),
    indent(1),
    cr(),
    emit_cases(Cases),
    indent(-1),
    cr(),
    p("end");

e( ?t('catch',{Expr}) ) ->
    p("catch "),
    e(Expr);

% receive

e( ?t('receive',{Cases}) ) ->
    p("receive"),
    indent(1),
    cr(),
    emit_cases(Cases),
    indent(-1),
    cr(),
    p("end");

% case

e( ?t('case',{Expr,Cases}) ) ->
    p("case "),
    e(Expr),
    p(" of"),
    indent(1),
    cr(),
    emit_cases(Cases),
    indent(-1),
    cr(),
    p("end");

% cond

e( ?t('cond',Cases) ) ->
    p("if"),
    indent(1),
    cr(),
    emit_cases(Cases),
    indent(-1),
    cr(),
    p("end");

% if

e( ?t('if',Cases) ) ->
    cr(),
    emit_if(Cases);



e( ?t('begin',{Block,__VarDefs}) ) ->
    p("begin"),
    indent(1),
    cr(),
    %e(Expr),
    emit_block(Block),
    indent(-1),
    cr(),
    p("end");

% where
% ( fun(A,B)-> Expr end )( A-Block,B-Block )

e( ?t('where',{Block,VarDefs}) ) ->
    Vars = lists:map( fun({Var,Block})->Var end,VarDefs ),
    p("( fun("),
    emit_list(Vars),
    p(") -> "),
    indent(1),
    cr(),
    %e(Expr),
    emit_block(Block),
    indent(-1),
    cr(),
    p("end)("),
    emit_list( lists:map( fun({Var,Block})->Block end,VarDefs ) ),
    p(")");

% function call, method call

e( ?t(funcall,{ ?t(objpath,Objpath),Params}) )->
    [Obj,Method] = Objpath,
    case Obj of
    ?t(symbol,Sym) ->
        p("~s:",[Sym]),
        e(Method),
        p("("),
        emit_list(Params),
        p(")");
    _ ->
        case type(Obj) of
            list -> p("eos:invoke_list(");
            _ -> p("eos:invoke(")
        end,
        e(Obj),
        p(","),
        e(Method),
        p(",["),
        emit_list(Params),
        p("])")
    end;

e( ?t(funcall,{Name,Params}) )->
    p( resolve_namespace(Name) ),
    p("("),
    emit_list(Params),
    p(")");

% pipe

e( ?t(pipe,[A,B]) )->
    Expr = case B of
        ?token(LnRw,funcall,{X,Param}) ->
            ?token(LnRw,funcall,{X,Param++[A]});

        ?token(LnRw,_) = X ->
            %io:format("pipe ~p\n",[X]),
            ?token(LnRw,funcall,{X,[A]})
    end,

    %io:format("@@@@@ ~p\n",[Expr]),
    e(Expr);


% assignment

e( ?t(':=',{ ?t(objpath,Objpath),B}) )->
    [Obj,Slot] = Objpath,
    p("eos:set_slot("),
    e(Obj),
    p(","),
    e(Slot),
    p(" , "),
    e(B),
    p(")");

e( ?t(':=',{ ?t(globalvar,Varname),B}) )->
    p("put('$~s' , ",[Varname]),
    e(B),
    p(")");

e( ?t(':=',{ ?t(membervar,Varname),B}) )->
    p("eos:set_slot(This,'~s' , ",[Varname]),
    e(B),
    p(")");

e( ?token(LnRw,':=',{A,B}) )->
    erleos:compile_error( compile_error,LnRw,eosstd:fmt("variable assignment is only permmited to object and global variables.: ~p :=",[A]) );


%

e( ?t('=',{A,B}) )->
    e(A),
    p(" = "),
    e(B);

e( ?t(quotedsymbol,X) )->
    p("'~s'",[X]);

e( ?t(symbol,X) )->
    p(X);

e( ?t(number,X) )->
    p("~w",[X]);

e( ?t(n_int,{N,X}) )->
    p("~w#~s",[N,X]);

e( ?t(e_int,{X,N}) )->
    p("~we~w",[X,N]);

% R:8 
e( ?t(var_ext,{X,Ext}) )->
    %io:format("###var_ext1 = ~p\n",[X]),
    %io:format("###var_ext2 = ~p\n",[Ext]),
    p("~s:",[X]),
    e(Ext);

% reference immutable local variable
e( ?t(var,X) )->
    p("~s",[X]);

% reference global(process,object) variable
e( ?t(globalvar,X) )->
    p("get('$~s')",[X]);

e( ?t(objpath,[Obj,Member]) )->
    p("eos:get_slot("),
    e(Obj),
    p(","),
    e(Member),
    p(")");

e( ?t(membervar,X) )->
    p("eos:get_slot(This,'~s')",[X]);

e( ?t(rec_modify,{Var,RecordName,KeyValues} ) )->
    %io:format("*******rec_modify ~p ~p ~p\n",[Var,RecordName,KeyValues] ),
    e(Var),
    p("#~s{",[RecordName]),
    emit_dictionary(KeyValues),
    p("}");

e( ?t(recpath,{Obj,Rec,Member}) )->
    %p("~s#~s.~s",[Obj,Rec,Member]);
    p("("),
    e(Obj),
    e(Rec),
    p("."),
    e(Member),
    p(")");

e( ?t(record_name,RecordName) )->
    p("#~s",[RecordName]);


%e( ?t(bop,A) )->
%    p(" ~s ",[A]);

e( ?t('not',A) )->
    p("not "),
    e(A);

e( ?t(',') )->
    p(", ");

e( ?t('when') )->
    p(" when ");

e( ?t('of') )->
    p(" of");

e( ?t('record',{Name,KeyValues}) ) ->
    p("#~s{",[Name]),
    emit_dictionary(KeyValues),
    p("}");


e( ?t(A) ) when is_atom(A) ->
    p(A);

e( ?t(A) )->
    pp(A);

e(List) when is_list(List) ->
    emit(List);

e(X)->
    io:format(" ?????~p\n",[X]).

%

emit([])->
    [];
emit([H|T])->
    e(H),
    p(" "),
    emit(T).

emit_block([])->
    [];
emit_block([H])->
    e(H);
emit_block([H|T])->
    e(H),
    p(","),
    cr(),
    emit_block(T).

%

emit_function([ ?t(defun,{Decl,Name,Arity,Params,Body})|T ] ,{Name,Arity},Count)->
    if  Count =/= 0 -> p(";"),cr();
        true -> []
    end,
    cr(),
    emit_decl(Decl),
    p("~s(",[Name]),
    emit_list(Params),
    p(") ->"),
    indent(1),
    cr(),
    emit_block(Body),
    indent(-1),

    emit_function(T,{Name,Arity},Count+1);

emit_function([ ?t(defun2,{Decl,Name,Arity,Params,Guard,Body})|T ] ,{Name,Arity},Count)->
    if  Count =/= 0 -> p(";"),cr();
        true -> []
    end,
    cr(),
    emit_decl(Decl),
    p("~s(",[Name]),
    emit_list(Params),
    p(") when "),
    emit(Guard),
    p(" ->"),
    indent(1),
    cr(),
    emit_block(Body),
    indent(-1),

    emit_function(T,{Name,Arity},Count+1);

%

emit_function([ ?t(defmemberfun,{Decl,Name,Arity,Params,Body})|T ] ,{Name,Arity},Count)->
    if  Count =/= 0 -> p(";"),cr();
        true -> []
    end,
    cr(),
    emit_decl(Decl),
    p("~s(This,Arguments)->",[Name]),
    indent(1),
    cr(),

    p("["),
    emit_list2(Params),
    p("|RestArguments] = Arguments,"),
    cr(),
    emit_block(Body),
    indent(-1),

    emit_function(T,{Name,Arity},Count+1);

emit_function([ ?t(defmemberfun2,{Decl,Name,Arity,Params,Guard,Body})|T ] ,{Name,Arity},Count)->
    if  Count =/= 0 -> p(";"),cr();
        true -> []
    end,
    cr(),
    emit_decl(Decl),
    p("~s(This,Arguments) when ",[Name]),
    emit(Guard),
    p(" ->"),
    indent(1),
    cr(),

    p("["),
    emit_list2(Params),
    p("|RestArguments] = Arguments,"),
    cr(),
    emit_block(Body),
    indent(-1),

    emit_function(T,{Name,Arity},Count+1);

emit_function(Src,_,_) ->
    p("."),
    cr(),
    endform(),
    Src.

%
count(Delimiter,[],Sum)->
    %io:format("***A ~p\n",[Sum]),
    Sum;

count(Delimiter,[?t(Delimiter)|T],Sum)->
    %io:format("***C ~p\n",[Delimiter]),
    count(Delimiter,T,Sum+1);

count(Delimiter,[H|T],Sum)->
    %io:format("***B ~p\n",[T]),
    count(Delimiter,T,Sum).

count(Delimiter,List)->
    %io:format("***D ~p\n",[List]),
    count(Delimiter,List,0).

spec([],_)->
    [];

spec([?t('->')|T],1)->
    %io:format("***-> ~p\n",[0]),
    p(") -> "),
    spec(T,-1);

spec([?t('->')|T],Arity)->
    %io:format("***-> ~p\n",[Arity]),
    p(","),
    spec(T,Arity-1);

spec([?t(var,'Unit')|T],Arity)->
    spec(T,Arity);

spec([?t(var,Name)|T],Arity)->
    p( to_lower(Name) ),
    p("()"),
    spec(T,Arity);

spec([H|T],Arity)->
    e(H),
    spec(T,Arity).


emit_spec(Name,Spec)->
    cr(),
    Arity = count('->',Spec),
    %io:format("***spec = ~p --- ~p\n",[Spec,Arity]),
    p("- spec ~s",[Name]),
    p("("),
    spec(Spec,Arity),
    p("."),
    cr().



%

e_decl( ?t(funspec,{Name,Spec}) )->
    emit_spec(Name,Spec).

emit_decl([])->
    undefined;

emit_decl([H|T])->
    e_decl(H),
    emit_decl(T).


%

emit_toplevel([])->
    true;

emit_toplevel([ ?t(definition,{Name,Param})|T] )->
    p("- "),
    e(Name),
    p("("),
    %e(Param),
    emit_list(Param),
    p(")."),
    cr(),
    endform(),
    emit_toplevel(T);

emit_toplevel([ ?t(defun,{Decl,Name,Arity,Params,Body})|T ]=Src )->
    emit_toplevel( emit_function(Src,{Name,Arity},0) );

emit_toplevel([ ?t(defun2,{Decl,Name,Arity,Params,Guard,Body})|T ]=Src )->
    emit_toplevel( emit_function(Src,{Name,Arity},0) );

emit_toplevel([ ?t(defmethod,{Decl,Name,Arity,Params,Body})|T ]=Src )->
    emit_toplevel( emit_function(Src,{Name,Arity},0) );

emit_toplevel([ ?t(defmethod2,{Decl,Name,Arity,Params,Guard,Body})|T ]=Src )->
    emit_toplevel( emit_function(Src,{Name,Arity},0) );

emit_toplevel( [H|T] )->
    pp(H),
    cr(),
    emit_toplevel(T).

init()->
    put(outstr,fun(S)->io:format("~s",[S]) end).

init(F)->
    put(outstr,F).

%

exec_emit_block(HdSrc,Src)->
    erleos_gensym:start(),
    put(cur_form,HdSrc),
    put(forms,[]),
    put(indent,0),
    put(row,0),
    put(rowbase,0),
    emit_block(Src),
    get(cur_form).


exec_emit(HdSrc,Src)->
    erleos_gensym:start(),
    put(cur_form,HdSrc),
    put(forms,[]),
    put(indent,0),
    put(row,0),
    put(rowbase,0),
    emit_toplevel(Src).

compile(HdSrc,Src)->
    exec_emit(HdSrc,Src),
    lists:reverse( get(forms) ).

%

