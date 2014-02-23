-module(erleos_parser).
-compile(export_all).
-include("erleos_parser.hrl").
-include("eos_modobj.hrl").

init_module_object()->
    [].

lnrw( [] )->
     {0,0};

lnrw( {{Line,Row},_} )->
     {Line,Row};

lnrw([ {{Line,Row},_} |_])->
     {Line,Row};

lnrw([[ {{Line,Row},_} |_]|_])->
     {Line,Row}.

lnrw_row(X)->
    {_,Row} = lnrw(X),
    Row.

lnrw_line(X)->
    {Line,_} = lnrw(X),
    Line.

count_arity(Param)->
    length(Param).

test_count_arity()->
    0 = count_arity([]),
    1 = count_arity([x]),
    2 = count_arity([x,{lnrw,','},y]),
    3 = count_arity([x,{lnrw,','},y,{lnrw,','},z]).



% (Paren) -> {paren,[Paren]}
% {Tuple} -> {tuple,[Tuple]}
% [List] -> {list,[List]}
%   ETS}>> -> {etsinit,[ETS]}
% module:fun -> {path,[module,fun]}
% Obj.member -> {objpath,[Obj,member]}

% { LineRow,Type,Param }
% { LineRow,Type,{Param,Param} }

% {symbol,'symbol'}
% {number,123}
% {var,'varname'}
% 

trace(Tag,A)->
    io:format("~s: ~p\n",[Tag,A]).

% 新しくトークンを1つ読む。今までのcurtokenを返す
next()->
    eos_var:lunshift(back,get(cur_token) ),
    %%io:format("next >> ~p\n",[get(back)]),
    %%io:format("tkn: ~p\n",[get(cur_token)]),

    NewToken = eos_var:lshift(src),
    %trace(next,NewToken),
    put( cur_token,NewToken ).

back()->
    eos_var:lunshift(src,cur_token()),

    NewToken = eos_var:lshift(back),
    %%io:format("back >> ~p\n",[get(back)]),
    put( cur_token,NewToken ).

restsrc()->
    eos_var:lget(src).



% 次のトークンを見る
next_token()->
    eos_var:ltop(src).

% 現在のトークンを見る
cur_token()->
%    A = get( cur_token ),
%    case A of
%        undefined -> 
%            erleos:compile_error(parser,[],eosstd:fmt("unexpected end of code"));
%        _ -> A
%    end.
    get( cur_token ).

%accept(Target)->
%   {LnRw,Token = eos_var:ltop(src),
%   case Target == Token of
%       true -> eos_var:lshift(src),
%               true;
%       X -> false
%   end.

accept_type(Type)->
    {LnRw,Token} = eos_var:ltop(src),
    case Token of
        {Type,_} -> eos_var:lshift(src),
                    true;
        _ -> false
    end.

may_exist(Type)->
    case cur_token() of
        ?token(LnRw,Type) -> next();
        _ -> []
    end.

expect(Type)->
    case cur_token() of
        ?token(LnRw,Type) -> next();
        ?token(LnRw,_) = X ->
            erleos:compile_error(expect,LnRw,eosstd:fmt("~p is expected, but ~p",[Type,X]));
        X ->
            erleos:compile_error(expect,{-1,-1},eosstd:fmt("~p is expected, but ~p",[Type,X]))
    end.

expect(Type,Param)->
    case cur_token() of
        ?token(LnRw,Type,Param) -> next();
        ?token(LnRw,_) = X ->
            erleos:compile_error(expect,LnRw,eosstd:fmt("~p : ~p is expected, but ~p",[Type,Param,X]))
    end.

expect_type(Type)->
    case cur_token() of
        ?token(LnRw,{Type,_} ) -> next();
        ?token(LnRw,_) = X ->
            erleos:compile_error(expect,LnRw,eosstd:fmt("~p is expected, but ~p",[Type,X]))
    end.

emit(X)->
    %trace(emit,X),
    eos_var:lunshift(emit,X).


symbolof( ?token(LnRw,symbol,Sym) )->
    Sym;
symbolof( ?token(LnRw,quotedsymbol,Sym) )->
    Sym.

modtoken( ?token(LnRw,symbol,Sym),NewSym )->
    ?token(LnRw,symbol,eosstd:to_atom(NewSym) );
modtoken( ?token(LnRw,quotedsymbol,Sym),NewSym )->
    ?token(LnRw,quotedsymbol,eosstd:to_atom(NewSym) ).

%
% param (Expr,Expr..)
% list [Expr,Expr..]
% tuple {Expr,Expr..}
% dictionary
% record
%

% X,X|
parse_block_param(Acc)->
    A = expression(),
    %io:format("***** parse_block_param: ~p\n",[A]),
    %trace(aa,A),
    case next() of
        ?token(_,'|') ->
            lists:reverse([A|Acc]);

        ?token(_,',') ->
            parse_block_param([A|Acc])
    end.


% X,X|
parse_block_param()->
    %io:format("***** parse_block_param\n"),
    case cur_token() of
        % empty param
        ?token(_,'|') ->
            next(),
            [];

        X ->
            parse_block_param([])
    end.


% X,X)
parse_param_(Acc,Row)->
    A = expression(),
    %io:format("parse_param expression = ~p\n",[A]),
    %trace(aa,A),
    case next() of
        ?token(_,')') ->
            lists:reverse([A|Acc]);

        ?token(_,',') ->
            parse_param_([A|Acc],Row);

        ?token({_,Row},_) = B ->
            back(),
            parse_param_([A|Acc],Row)
    end.


% X,X)
parse_param()->
    Cur = cur_token(),
    {_,Row} = LnRw = lnrw(Cur),
    try
        case Cur of

            % empty param
            ?token(_,')') ->
                next(),
                [];

            X ->
                parse_param_([],Row)
        end
    catch
        _:_ ->
            erleos:compile_error(parser,LnRw,eosstd:fmt("error while parsing param",[]))
    end.

%
%


parse_list_cons(List)->
    CDR = expression(),
    expect(']'),
    ?token( undefined,list_cons,{List,CDR}).

parse_list_comprehension(A)->
    {_,Row} = lnrw( cur_token() ),
    B = parse_list_([],Row),
    ?token( undefined,list_comprehension,{A,B}).

parse_list_sequence(A)->
    B = expression(),
    expect(']'),
    ?token( undefined,list_sequence,{A,B}).

parse_list_sequence2(A)->
    B = expression(),
    expect(']'),
    ?token( undefined,list_sequence2,{A,B}).

% X,X)
parse_list_(Acc,Row)->
    A = expression(),
    %%io:format("A = ~p\n",[A]),
    %trace(aa,A),
    case next() of
        ?token(_,']') ->
            lists:reverse([A|Acc]);

        % cons,rest of list
        ?token(_,'|') = B ->
            %%io:format("*** list comprehension\n"),
            parse_list_cons(lists:reverse([A|Acc]));

        ?token(_,',') ->
            parse_list_([A|Acc],Row);

        ?token({_,Row},_) = B ->
            back(),
            parse_list_([A|Acc],Row)
    end.


% X,X)
parse_list()->
    Cur = cur_token(),
    LnRw = lnrw(Cur),
    try
        case Cur of
            % empty list
            ?token(_,']') ->
                next(),
                [];

            _ ->
                A = expression(),
                {_,Row} = lnrw(A),
                %%io:format("A = ~p\n",[A]),
                %trace(aa,A),
                case next() of
                    % single element list
                    ?token(_,']') ->
                        [A];

                    % list comprehension
                    ?token(_,'||') = B ->
                        %%io:format("*** list comprehension\n"),
                        parse_list_comprehension(A);

                    ?token(_,'..') = B ->
                        parse_list_sequence(A);

                    ?token(_,'...') = B ->
                        parse_list_sequence2(A);

                    ?token(_,'|') ->
                        parse_list_cons([A]);

                    ?token(_,',') ->
                        parse_list_([A],Row);

                    ?token({_,Row},_) = B ->
                        back(),
                        parse_list_([A],Row)
                end

        end
    catch
        _:_ ->
            erleos:compile_error(parser,LnRw,eosstd:fmt("error while parsing list",[]))
    end.


%
%

%
%

% X,X)
parse_binary_(Acc,Row)->
    A = expression(),
    %%io:format("A = ~p\n",[A]),
    %trace(aa,A),
    case next() of
        ?token(_,'>>') ->
            lists:reverse([A|Acc]);

        ?token(_,'||') = B ->
            %%io:format("*** list comprehension\n"),
            parse_binary_([A,'||'|Acc],Row);

        ?token(_,'..') = B ->
            %%io:format("*** list sequence\n"),
            parse_binary_([A,'..'|Acc],Row);

        ?token(_,'...') = B ->
            %%io:format("*** list sequence\n"),
            parse_binary_([A,'...'|Acc],Row);

        ?token(_,',') ->
            parse_binary_([A|Acc],Row);

        X ->
            %%io:format("X = ~p\b",[X]),
            {_,Row} = lnrw(X),
            back(),
            parse_binary_([A|Acc],Row)
    end.


% X,X)
parse_binary()->
    Cur = cur_token(),
    LnRw = lnrw(Cur),
    try
        case Cur of
            % )
            ?token(_,'>>') ->
                next(),
                [];

            X ->
                {_,Row} = lnrw(X),
                parse_binary_([],Row)
        end
    catch
        _:_ ->
            erleos:compile_error(parser,LnRw,eosstd:fmt("error while parsing binary",[]))
    end.




% X,X)
parse_tuple_(Acc,Row)->
    A = expression(),
    %trace(aa,A),
    case next() of
        ?token(_,'}') ->
            lists:reverse([A|Acc]);

        ?token(_,',') ->
            parse_tuple_([A|Acc],Row);

        X ->
            {_,Row} = lnrw(X),
            back(),
            parse_tuple_([A|Acc],Row)
    end.


% X,X)
parse_tuple()->
    Cur = cur_token(),
    LnRw = lnrw(Cur),
    try
        case Cur of
            % )
            ?token(_,'}') ->
                next(),
                [];

            X ->
                {_,Row} = lnrw(X),
                parse_tuple_([],Row)
        end
    catch
        _:_ ->
            erleos:compile_error(parser,LnRw,eosstd:fmt("error while parsing tuple",[]))
    end.

%
%


parse_pat_block_(Name,Terminator,Delimiter,BlockRow,Acc)->
    %io:format("pat_block cur = ~p\n",[cur_token()]),
    case cur_token() of
        [] -> lists:reverse(Acc);
        ?token(_,Terminator) -> lists:reverse(Acc);
        CurToken ->
            {_,Row} = lnrw( CurToken ),
            if Row =/= BlockRow -> lists:reverse(Acc);
                true ->
                    Pattern = extract_until(Delimiter),
                    %io:format("--- pattern = ~p\n",[Pattern]),
                    Block = parse_block(Name),
                    %io:format("--- block = ~p\n",[Block]),
                    parse_pat_block_(Name,Terminator,Delimiter,BlockRow,[{Pattern,Block}|Acc])
            end
    end.

parse_pat_block(Name,Terminator,Delimiter)->
    Cur = cur_token(),
    LnRw = lnrw(Cur),
    {_,Row} = lnrw( Cur ),

    try
        %trace(case_row,Row),
        parse_pat_block_(Name,Terminator,Delimiter,Row,[])
    catch
        _:_ ->
            erleos:compile_error(parser,LnRw,eosstd:fmt("error while parsing ~s",[Name]))
    end.

parse_pat_block(Name,Delimiter)->
    parse_pat_block(Name,undefined,Delimiter).



%
%

%parse_dic(Terminator)->
%    parse_pat_block(Terminator,'=').


%
%


parse_record_(Name,Terminator,Delimiter,BlockRow,Acc)->
    %io:format("@ ~p\n",[ cur_token() ]),
    case cur_token() of
        []            -> lists:reverse(Acc);
        ?token(_,'}') ->
            lists:reverse(Acc);

        ?token(_,',') ->
            next(),
            Pattern = extract_until(Delimiter),
            %io:format("--- pattern = ~p\n",[Pattern]),
            Block = parse_block(Name,','),
            %io:format("--- block = ~p\n",[Block]),
            parse_record_(Name,Terminator,Delimiter,BlockRow,[{Pattern,Block}|Acc]);

        CurToken ->
            {_,Row} = lnrw( CurToken ),
            %io:format("@@@ BlockRow = ~p, Row = ~p, CurToken = ~p, \n",[BlockRow,Row,CurToken]),
            if Row =/= BlockRow -> lists:reverse(Acc);
                true ->
                    Pattern = extract_until(Delimiter),
                    %io:format("--- pattern = ~p\n",[Pattern]),
                    Block = parse_block(Name,','),
                    %io:format("--- block = ~p\n",[Block]),
                    parse_record_(Name,Terminator,Delimiter,BlockRow,[{Pattern,Block}|Acc])
            end
    end.

parse_record(Name,Terminator,Delimiter)->
    Cur = cur_token(),
    LnRw = lnrw(Cur),
    {_,Row} = lnrw( Cur ),
    try
        %trace(case_row,Row),
        parse_record_(Name,Terminator,Delimiter,Row,[])
    catch
        _:_ ->
            erleos:compile_error(parser,LnRw,eosstd:fmt("error while parsing ~s",[Name]))
    end.

parse_record(Name,Delimiter)->
    parse_record(Name,undefined,Delimiter).



%
%


parse_monad()->
    CurToken = cur_token(),
    LnRw = lnrw(CurToken),
    Block = parse_block(<<"monad">>),
    ?token( LnRw,'monad',Block ).




%
%

parse_paren()->
    A = expression(),
    expect(')'),
    A.

%
%

parse_if2(Acc)->
    CurToken = cur_token(),
    LnRw = lnrw( CurToken ),

    case CurToken of
        ?token(_,'else') ->
            next(),
            Block = parse_block(<<"else">>),
            ?token( LnRw,'if',Acc++[Block] );
        ?token(_,'elif') ->
            next(),
            parse_if_(Acc);
        _ ->
            ?token( LnRw,'if',Acc )
    end.

parse_if_(Acc)->
    Cond = expression(),
    expect('then'),
    Block = parse_block(<<"if">>),
    parse_if2(Acc++[Cond,Block]).

parse_if()->
    Cur = cur_token(),
    LnRw = lnrw(Cur),
    try
        parse_if_([])
    catch
        _:_ ->
            erleos:compile_error(parser,LnRw,eosstd:fmt("error if clause",[]))
    end.

%
%


parse_cases(Name)->
    parse_pat_block(Name,'->').

%
%

parse_binds_(Name,Terminator,BlockRow,Acc)->
    case cur_token() of
        [] -> lists:reverse(Acc);
        CurToken ->
            {_,Row} = lnrw( CurToken ),
            if Row =/= BlockRow -> lists:reverse(Acc);
                true ->
                    Var = extract_until('='),
                    Block = parse_block(Name,Terminator),
                    parse_binds_(Name,Terminator,BlockRow,[{Var,Block}|Acc])
            end
    end.

parse_binds(Name,Terminator)->
    {_,Row} = lnrw( cur_token() ),
    %trace(case_row,Row),
    parse_binds_(Name,Terminator,Row,[]).

parse_binds(Name)->
    parse_binds(Name,undefined).

%
%

parse_fun_(Name,{Line,Row})->
    expect('('),
    Param = parse_param(),

    case cur_token() of
        ?token(_,'->') ->
            next(),
            Body = parse_block(Name,'end'),
            may_exist('end'),
            {'fun',count_arity(Param),Param,Body};

        ?token(_,'when') ->
            next(),
            When = extract_until('->'),
            Body = parse_block(Name,'end'),
            may_exist('end'),
            {'fun2',count_arity(Param),Param,When,Body}
    end.


parse_funs_(Name,{Line,Row}=LnRw,Acc)->
    case cur_token() of
        ?token({_,Row},'(') -> 
            parse_funs_(Name,LnRw,[parse_fun_(Name,LnRw)|Acc]);
        _ ->
            ?token( LnRw,'fun',lists:reverse(Acc) )
    end.

parse_fun(Name,LnRw)->
    ParamLnRw = lnrw( cur_token() ),
    ?token( _   ,'fun',Fundef ) = parse_funs_(Name,ParamLnRw,[]),
    ?token( LnRw,'fun',Fundef ).

parse_method(Name,LnRw)->
    ?token( LnRw,'fun',Fundef ) = parse_fun(Name,LnRw),
    ?token( LnRw,'method',Fundef ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%paren( ?token(LnRw,'<<{') )->
%    %KeyValues = parse_dic('}>>'),
%    KeyValues = parse_record(<<"dictionary">>, '}>>','='),
%    expect('}>>'),
%    ?token(LnRw,'dictionary',KeyValues);

paren( ?token(LnRw,'#{') )->
    %KeyValues = parse_dic('}'),
    KeyValues = parse_record(<<"proplists">>, '}','='),
    expect('}'),
    ?token(LnRw,'proplist',KeyValues);

paren( ?token(LnRw,'#<') )->
    [TypeName] = extract_until('>'),
    Param = case cur_token() of
        ?t('(') ->
            next(),
            parse_param();

        _ -> []
    end,
    expect('{'),
    KeyValues = parse_record(<<"newobj">>, '}','='),
    expect('}'),
    ?token(LnRw,'newobj',{modtoken(TypeName,eosstd:fmt_atom("eos@~s",[symbolof(TypeName)])),Param,KeyValues});

paren( ?token(LnRw,'(') )->
    Param = parse_paren(),
    %io:format("****paren = ~p\n",[Param]),
    ?token(LnRw,'paren',Param);

paren( ?token(LnRw,'[') )->
    Res = parse_list(),

    %io:format("list = ~p\n",[List]),

    if is_list(Res) -> ?token(LnRw,'list',Res );
       true ->
            ?token(_,Type,Param) = Res,
            ?token(LnRw,Type,Param)
    end;

paren( ?token(LnRw,'<<') )->
    List = parse_binary(),

    %io:format("list = ~p\n",[List]),

    case List of
        ['||'|Rest] ->
            %%io:format("@@@@ ~p",[List]),
            ?token(LnRw,'binary_comprehension',Rest );
        ['..'|Rest] ->
            %%io:format("@@@@ ~p",[List]),
            ?token(LnRw,'binary_sequence',Rest );
        ['...'|Rest] ->
            %%io:format("@@@@ ~p",[List]),
            ?token(LnRw,'binary_sequence2',Rest );
        _ ->
            ?token(LnRw,'binary',List )
    end;


paren( ?token(LnRw,'{') )->
    List = parse_tuple(),
    ?token(LnRw,'tuple',List);

paren( ?token(LnRw,record_name,Name) )->
    expect('{'),
    KeyValues = parse_record(<<"record">>, '='),
    expect('}'),
    ?token(LnRw,'record',{Name,KeyValues});

paren(X)->
    X.


% Variable::type
typed(A)->
    %A = cur_token(),
    %next(),
    LnRw = lnrw(A),
    {Line1,Row1} = LnRw,

    case cur_token() of
        ?token({Line2,Row2},'::') ->
            next(),
            B = cur_token(),
            next(),
            ?token(LnRw,typed,[B,A]);

        _ ->
            A
    end.


% module:function
path(A)->
    %A = cur_token(),
    %next(),
    LnRw = lnrw(A),
    {Line1,Row1} = LnRw,

    case cur_token() of
        %?token({Line2,Row2},'#') = M->
        %    next(),
        %    B = cur_token(),
        %    next(),
        %    [A,M,B];

        %% Varname#Record.member
        ?token({Line2,Row2},record_name,RecordName) = Rec->
            %io:format("**** Varname#Record.member\n"),
            next(),
            case cur_token() of
                ?t('.') ->
                    next(),
                    B = cur_token(),
                    next(),
                    ?token(LnRw,recpath,{A,Rec,B});

                ?t('{') ->
                    next(),
                    KeyValues = parse_record(<<"modify record">>, '}','='),
                    %io:format("**** KEYVALS = ~p\n",[KeyValues]),
                    expect('}'),
                    ?token(LnRw,'rec_modify',{A,RecordName,KeyValues})

            end;

        ?token({Line2,Row2},':') ->
            next(),
            B = cur_token(),
            next(),
            ?token(LnRw,path,[A,B]);

        %% object.member
        ?token({Line2,Row2},'.') ->
            next(),
            if (Line1==Line2) or (Row1==Row2) ->
                B = cur_token(),
                next(),
                ?token(LnRw,objpath,[A,B]);
            %                
            true ->
                    A
            end;

        _ ->
            A
    end.

read_paren()->
    A = cur_token(),
    next(),
    paren(A).


read_typed()->
    A = read_paren(),
    typed(A).

read_path()->
    A = read_typed(),
    path(A).

%
%

factor( ?token(LnRw,'while') )->
    %io:format("$$$$ begin while \n"),

    Cond = expression(),
    %io:format("$$$$ while = ~p\n",[Res]),
    %%?token(_,dokeyword,{Cond,Block}) = Res,
    %io:format("$$$$ while cond = ~p\n",[Cond]),
    %io:format("$$$$ while block = ~p\n",[Block]),
    expect('do'),
    Block = parse_block(<<"while block">>),
    ?token(LnRw,'while',{Cond,Block});

factor( ?token(LnRw,'monad') )->
    Monad = parse_block(<<"monad block">>),
    io:format("$$$$ Monad = ~p\n",[Monad]),
    %expect('do'),
    Block = parse_block(<<"monad body">>),
    ?token(LnRw,'monad',{Monad,Block});

factor( ?token(LnRw,'catch') )->
    Expr = expression(),
    ?token(LnRw,'catch',{Expr});

factor( ?token(LnRw,'try') )->
    TryBlock = parse_block(<<"try">>),
    expect('catch'),
    Cases = parse_cases(<<"try cases">>),
    Cases2 = lists:map(
        fun({Pat,Block})->
            %io:format("=== ~p : ~p\n",[Pat,Block]),
            case Pat of
                [AA,?token(LnRw2,'.'),BB] ->
                    {[AA,?token(LnRw2,':'),BB],Block};
                _ -> {Pat,Block}
            end
        end
        ,Cases
    ),
    ?token(LnRw,'try',{TryBlock,Cases2});

factor( ?token(LnRw,'receive') )->
    Cases = parse_cases(<<"receive cases">>),
    ?token(LnRw,'receive',{Cases});

factor( ?token(LnRw,'let') )->
    VarDefs = parse_binds(<<"let">>, 'in'),
    expect('in'),

    Block = parse_block(<<"let block">>),
    ?token(LnRw,where,{Block,VarDefs});

%%% factor( ?token(LnRw,'local') )->
%%%     Block = parse_block('end'),
%%%     may_exist('end'),
%%%     ?token(LnRw,where,{Block,[]});

factor( ?token(LnRw,'begin') )->
    Block = parse_block(<<"begin block">>, 'end'),
    may_exist('end'),
    ?token(LnRw,'begin',{Block,[]});

factor( ?token(LnRw,'lambda') )->
    Fun = parse_fun(<<"lambda>">>,LnRw),
    expect(')'),
    Fun;

factor( ?token(LnRw,'fun') )->
    case cur_token() of
        ?token(_,symbol,_) ->
            Sym = expression(),
            ?token(LnRw,funref,Sym);
        _ ->
            parse_fun(<<"fun">>,LnRw)
    end;

factor( ?token(LnRw,'method') )->
    parse_method(<<"method">>,LnRw);

factor( ?token(LnRw,'case') )->
    Expr = expression(),
    expect('of'),

    Cases = parse_cases(<<"case">>),
    ?token( LnRw,'case',{Expr,Cases} );

factor( ?token(LnRw,'cond') )->
    Cases = parse_cases(<<"cond">>),
    ?token( LnRw,'cond',Cases );

factor( ?token(LnRw,'if') )->
    parse_if();

factor( ?token(LnRw,'not') )->
    Expr = expression(),
    ?token( LnRw,'not',Expr );

factor(?token({Line,Row},_)=AA)->
    case cur_token() of

        %% 同じ行で、次が(であれば関数呼び出し。
        ?token({Line,_},'(') ->
            next(),
            Param = parse_param(),
            A = ?token( lnrw(AA),funcall,{AA,Param} ),
            case cur_token() of
                ?token({Line2,Row2},'do')  when Row =< Row2 ->
                    next(),
                    B = cur_token(),
                    BlockFunParam = case B of
                        ?token(_,'|') ->
                            next(),
                            parse_block_param();
                            %parse_param();
                        _ -> []
                    end,
                    Body = parse_block(<<"do block">>),
                    %io:format("do block: ~p\n",[Body]),

                    LnRw = lnrw(A),
                    Arity = count_arity(BlockFunParam),
                    BlockFun = ?token(LnRw,'fun',[{'fun',Arity,BlockFunParam,Body}]),

                    case A of
                        ?token(_,funcall,{X,Param}) -> 
                            ?token(LnRw,funcall,{X,[BlockFun|Param] });

                        ?token(_,objpath,[{X,Param}]) ->
                            ?token(LnRw,funcall,{A,[BlockFun] });

                        _ ->
                            ?token(LnRw,dokeyword,{A,Body})
                    end;

                _ -> A
            end;

        _ -> AA
    end;

factor(A)->
    A.



factor()->
    A = read_path(),
    factor( A ).

term()->
    B = factor(),
    term2(B).

term2(A)->
    {Line,Row} = lnrw(A),

    case cur_token() of
        ?token(_,'.') ->
            term2( factor() );

        %            

        _ -> A
    end.


%
%

priority2()->
    case cur_token() of
        ?token(LnRw,'-') ->
            next(),
            ?token(LnRw,'-',term());

        ?token(LnRw,'+') ->
            next(),
            ?token(LnRw,'+',term());

        _ ->
            A = term(),
            priority2(A)
    end.

priority2(A)->
    term2(A).

%
%

priority3()->
    A = priority2(),
    priority3(A).

priority3(A)->
    {Line,Row} = lnrw(A),

    case cur_token() of

        ?token({_,Row2}=LnRw,'|>') when Row =< Row2 ->
            next(),
            B = term(),
            erleos:assert( Row =< lnrw_row(B),lnrw(B),"after |>"),
            priority3( ?token(LnRw,pipe,[A,B]) );

        ?token({_,Row2}=LnRw,'<|') when Row =< Row2 ->
            next(),
            B = priority3(),

            %io:format("----- ~p\n",[A]),
            %io:format("***** ~p\n",[B]),

            erleos:assert( Row =< lnrw_row(B),lnrw(B),"after <|"),

            case A of
                ?token(_,funcall,{X,Param}) ->
                    priority2( ?token(LnRw,funcall,{X,[B|Param] }) );

                X ->
                    priority2( ?token(LnRw,funcall,{X,[B] }) )
            end;

        _ -> A
    end.
binop_( A,?token(_,'<<-')=B )->
    C = parse_block(<<"bind to">>),
    D = parse_block(<<"bind block">>),
    io:format("bind blockA = ~p\n",[A]),
    io:format("bind blockC = ~p\n",[C]),
    io:format("bind blockD = ~p\n",[D]),
    ?token( lnrw(A),'bind',{A,C,D} );


binop_( A,?token(_,'<-')=B )->  [A,B] ++ [binop()];

binop_( A,?token(_,'=')=B )->   ?token( lnrw(A),'=',{A,binop()} );
binop_( A,?token(_,':=')=B )->  ?token( lnrw(A),':=',{A,binop()} );

binop_( A,?token(_,'+')=B )->   [A,B] ++ [binop()];
binop_( A,?token(_,'-')=B )->   [A,B] ++ [binop()];
binop_( A,?token(_,'/')=B )->   [A,B] ++ [binop()];
binop_( A,?token(_,'*')=B )->   [A,B] ++ [binop()];
binop_( A,?token(_,'++')=B )->  [A,B] ++ [binop()];

%
binop_( A,?token(_,'=/=')=B )-> [A,B] ++ [binop()];
binop_( A,?token(_,'/=')=B )->  [A,B] ++ [binop()];
binop_( A,?token(_,'=:=')=B )-> [A,B] ++ [binop()];
binop_( A,?token(_,'==')=B )->  [A,B] ++ [binop()];
binop_( A,?token(_,'>=')=B )->  [A,B] ++ [binop()];
binop_( A,?token(_,'=<')=B )->  [A,B] ++ [binop()];

binop_( A,?token(_,'>')=B )->   [A,B] ++ [binop()];
binop_( A,?token(_,'<')=B )->   [A,B] ++ [binop()];

%

%binop_( A,?token(_,'bnot')=B )->     [A,B] ++ [binop()];
%binop_( A,?token(_,'not')=B )->      [A,B] ++ [binop()];

binop_( A,?token(_,'div')=B )->      [A,B] ++ [binop()];
binop_( A,?token(_,'rem')=B )->      [A,B] ++ [binop()];
binop_( A,?token(_,'band')=B )->     [A,B] ++ [binop()];
binop_( A,?token(_,'bor')=B )->      [A,B] ++ [binop()];
binop_( A,?token(_,'bxor')=B )->     [A,B] ++ [binop()];
binop_( A,?token(_,'bsl')=B )->      [A,B] ++ [binop()];
binop_( A,?token(_,'bsr')=B )->      [A,B] ++ [binop()];
binop_( A,?token(_,'and')=B )->      [A,B] ++ [binop()];
binop_( A,?token(_,'or')=B )->       [A,B] ++ [binop()];
binop_( A,?token(_,'xor')=B )->      [A,B] ++ [binop()];


% これがあると、パラメーターの中身は,で連結された１つのExpressionになってしまう
%expression(A,?token(_,',')=B )->
%   [A,B] ++ expression();

binop_(A,_)->
    back(),
    A.

binop()->
    A = priority3(),
    B = cur_token(),

    case B of
        [] -> A;
        _ ->
            %io:format("@@A = ~p\n",[A]),
            %io:format("@@B = ~p\n",[B]),

            {Line1,Row1} = lnrw(A),
            {Line2,Row2} = lnrw(B),

            if (Line1==Line2) or (Row1==Row2) ->
                next(),
                binop_(A,B);
            true ->
                A
            end
    end.
%
%


exexpr()->
    A = binop(),
    exexpr2(A).

exexpr2(A)->
    B = cur_token(),

    case B of
        [] -> A;
        _ ->
            %io:format("@@A = ~p\n",[A]),
            %io:format("@@B = ~p\n",[B]),

            LnRw = {Line1,Row1} = lnrw(A),
                   {Line2,Row2} = lnrw(B),

            if (Line1==Line2) or (Row1==Row2) ->
                case B of
%                    ?token(_,'|>') ->
%                        next(),
%                        C = binop(),
%                        io:format("******** |> A: ~p\nC: ~p\n",[A,C]),
%                        exexpr2( ?token(LnRw,pipe,[A,C]) );

                    _ ->
                        A
                end;

            true ->
                A
            end
    end.

expression()->
    % suntax error check
    {Line,_} = lnrw( cur_token() ),
    RestSrc = restsrc(),
    Two = case RestSrc of
        [A|_] -> [cur_token(),A];
        [] -> [cur_token(),undefined]
    end,
    Two2 = lists:map(
        fun(X)->
            case X of
                ?token({Line,_},symbol,_) -> true;
                ?token({Line,_},quotedsymbol,_) -> true;
                ?token({Line,_},number,_) -> true;
                ?token({Line,_},int,_) -> true;
                ?token({Line,_},float,_) -> true;
                ?token({Line,_},n_int,_) -> true;
                ?token({Line,_},e_int,_) -> true;
                ?token({Line,_},globalvar,_) -> true;
                ?token({Line,_},membervar,_) -> true;
                ?token({Line,_},var,_) -> true;
                _ -> false
            end
        end
        ,Two
    ),
    %io:format("syntax check ~p ~p\n",[Two,Two2]),
    case Two2 of
        [true,true] ->
            erleos:compile_error( compile_error,lnrw(RestSrc),eosstd:fmt("syntax error: ~p",[Two]) );
        _ -> undefined
    end,

    %
    exexpr().

parse()->
    emit( expression() ).

%
%
%

parse_block_(Terminator,Row,Acc)->
    case cur_token() of
        ?token( _,Terminator ) ->
            lists:reverse( Acc );

        ?token( {_,Row2},',' ) when Row =< Row2 ->
            next(),
            NextExpr = expression(),
            erleos:assert( Row =< lnrw_row(NextExpr),lnrw(NextExpr),"block ;" ),
            parse_block_(Terminator,Row,[NextExpr|Acc]);

        ?token({_,Row2},'where') when Row =< Row2 ->
            next(),
            VarDefs = parse_binds(<<"where">>),
            Block = lists:reverse(Acc),
            LnRw = lnrw(Block),
            [ ?token(LnRw,where,{ Block,VarDefs}) ];

        ?token( {_,Row},_ ) ->
            parse_block_(Terminator,Row,[expression()|Acc]);

        _ ->
            lists:reverse( Acc )
    end.



parse_block(Name,Terminator)->
    Cur = cur_token(),
    LnRw = lnrw(Cur),
    try
        {_,Row} = lnrw( Cur ),
        %trace(block_row,Row),
        parse_block_(Terminator,Row,[])
    catch
        _:_ ->
            erleos:compile_error(parser,LnRw,eosstd:fmt("error while parsing ~s",[Name]))
    end.

parse_block(Name)->
    parse_block(Name,undefined).


%
%
%

parse_line(Line,Acc)->
    case cur_token() of
        ?token( {Line,_},_ ) = H ->
            next(),
            parse_line(Line,[H|Acc]);

        _ ->
            lists:reverse( Acc )
    end.

parse_line()->
    {Line,_} = lnrw( cur_token() ),
    parse_line(Line,[]).



%
%
%
extract_until(Target,Acc)->
    case next() of
        []->
            lists:reverse(Acc);
        ?token(_,Target) ->
            lists:reverse(Acc);
        X -> extract_until(Target,[X|Acc])
    end.

extract_until(Target)->
    extract_until(Target,[]).

parse_toplevel( ?token(LnRw,'-') )->
    Name = expect_type(symbol),
    expect('('),
    Param = parse_param(),
    ?token(LnRw,definition,{Name,Param} );


parse_toplevel( ?token(LnRw,symbol,Name) )->
    case next() of
        ?token(_,'(')->
            Param = parse_param(),

            case cur_token() of
                ?token(_,'->') ->
                    next(),
                    Body = parse_block(<<"function body">>),
                    ?token( LnRw,defun,{Name,count_arity(Param),Param,Body} );

                ?token(_,'when') ->
                    next(),
                    When = extract_until('->'),
                    Body = parse_block(<<"function body">>),
                    ?token( LnRw,defun2,{Name,count_arity(Param),Param,When,Body} );

                X ->
                    erleos:compile_error( compile_error,lnrw(X),eosstd:fmt("-> or when is needed after function param, but ~p",[X]) )

            end;

        ?token(_,'::')->
            Spec = parse_line(),
            %%io:format("@@@spec = ~p\n",[Spec]),
            ?token( LnRw,funspec,{Name,Spec} );

        X ->
            erleos:compile_error( compile_error,lnrw(X),eosstd:fmt("( or :: is needed after toplevel function name, but ~p",[X]) )
    end;



parse_toplevel( ?token(LnRw,member) )->
    ?token(_,symbol,Name) = next(),
    expect('('),
    Param = parse_param(),

    case cur_token() of
        ?token(_,'->') ->
            next(),
            Body = parse_block(<<"member body">>),
            ?token( LnRw,defmemberfun,{Name,count_arity(Param),Param,Body} );

        ?token(_,'when') ->
            next(),
            When = extract_until('->'),
            Body = parse_block(<<"member body">>),
            ?token( LnRw,defmemberfun2,{Name,count_arity(Param),Param,When,Body} );

        X ->
            erleos:compile_error( compile_error,lnrw(X),eosstd:fmt("-> or when is needed after member param, but ~p",[X]) )
    end;

parse_toplevel( ?token(LnRw,'.')=X )->
    erleos:compile_error( compile_error,lnrw(X),eosstd:fmt("unexpected toplevel: ~p",[X]) );

parse_toplevel( ?token(LnRw,';')=X )->
    erleos:compile_error( compile_error,lnrw(X),eosstd:fmt("unexpected toplevel: ~p",[X]) );

parse_toplevel( ?token(LnRw,',')=X )->
    erleos:compile_error( compile_error,lnrw(X),eosstd:fmt("unexpected toplevel: ~p",[X]) );

parse_toplevel( X )->
    erleos:compile_error( compile_error,lnrw(X),eosstd:fmt("unexpected toplevel: ~p",[X]) ).

parse_toplevel()->
    parse_toplevel( next() ).



%
%
%

conv_([])->
    EmittedSrc = get(emit),
    lists:reverse(EmittedSrc);

conv_(Src)->
    %parse(),
    emit( parse_toplevel() ),
    conv_( cur_token() ).

conv(Src)->
    init(Src),

    %true.
    erleos:try_block( fun()->
        {ok,conv_(Src)}
    end,
    "parse"
    ).

init(Src)->
    put(src,Src),
    next().

%run_parse_block()->
%    erleos:try_block( fun()->
%        {ok,parse_block()}
%    end,
%    "parse_block"
%    ).




test()->
    A = 100,
    B = 200,
    ( fun(A,B)->A*2+B end)(10,5).

