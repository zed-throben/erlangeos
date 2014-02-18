-module(erleos).
-compile(export_all).
-include("erleos_parser.hrl").
-include("erleos.hrl").

assert(Bool,LnRw,Msg)->
    if not Bool ->
        compile_error(assertion_failed,LnRw,Msg);
        true -> []
    end.

print_lnrw(LnRw)->
    case LnRw of
        {Line,Row} -> eosstd:fmt("line:~p,row:~p",[Line+1,Row+1]);
        _ -> ""
    end.

compile_error(Err,LnRw,Msg)->
    eosstd:puts( eosstd:fmt("~p : ~s",[print_lnrw(LnRw),Msg]) ),
    throw( {Err,Msg} ).

try_block(F,Msg)->
    if ?EOSRELEASE->
            try
                F()
            catch
                A:B -> 
                    io:format("exception handled in ~s: ~p | ~p\n",[Msg,A,B]),
                    {err,{A,B}}
            end;

        true ->
            F()
    end.

f(F,P)->
    eosstd:lsformat(F,P).

to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_list(X) -> X.

to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_atom(X);
to_atom(X) when is_binary(X) -> binary_to_atom(X,utf8).


p(X)->
    io:format("~s~n",[X]).

pp(X)->
    io:format("~p~n",[X]).

cr()->
	io:format("\n").

use(Obj,F)->
    eos:addref(Obj),
    Ret = F(Obj),
    eos:release(Obj),
    Ret.


compile_(Module,Src,Options)->
    A = erleos_lexer:lex(Src),

    Verbose = lists:member(verbose,Options),

    if Verbose ->
        p("\n\ntoken -----------------------------------------------------------------------------\n"),
        pp(A);

        true -> []
    end,

    ErlSrc = use(erleos_parser:start(),
        fun(Parser)->
            {ok,P1} = erleos_parser:invoke(Parser,conv,[A]),
            P1
        end
    ),

    P1A = ErlSrc,

    if Verbose ->
        p("\n\nP1 -----------------------------------------------------------------------------\n"),
        pp(P1A),

        cr(),
        p("////////////////////////////////////////////////////////////////////////////////////"),
        cr();
        true -> []
    end,

    %
    %
    %

    %P1 = [ ?token({0,0},definition,{?token({0,0},symbol,import),[?token({0,0},symbol,eosstd)]} )|P1A ],
    P1 = P1A,

    ModifiedSrc = use(erleos_module_parser:start(),
        fun(ModParser)->
            erleos_module_parser:invoke(ModParser,parse_toplevel,[P1]),

            % if no functions are exported, export all functions
            Exports = erleos_module_parser:get_slot(ModParser,exports),
            ModifiedSrc1 =
                if Exports==undefined ->
                    [ ?token({0,0},definition,{?token({0,0},symbol,compile),[?token({0,0},symbol,export_all)]} )|P1 ];
                    true -> P1
                end,

            % if module name is not defined, assume source file name as module name
            SrcModuleName = erleos_module_parser:get_slot(ModParser,module),
            %io:format("module = ~s\n",[SrcModuleName]),

            ModifiedSrc2 =
                if SrcModuleName==undefined ->
                    [ ?token({0,0},definition,{?token({0,0},symbol,module),[?token({0,0},quotedsymbol,Module)]} )|ModifiedSrc1 ];
                    true -> ModifiedSrc1
                end

            %[ ?token({0,0},definition,{?token({0,0},symbol,import),[?token({0,0},symbol,erleos_std)]} )|ModifiedSrc2 ]
        end
    ),


    %io:format("modified src = ~p\n",[ModifiedSrc]),

    use( erleos_emit:start(),
        fun(Emitter)->
            %io:format("AAAAA\n"),
            erleos_emit:invoke(Emitter,init,[fun(X)->X end]),
            %io:format("BBBBB\n"),

            %HdSrc = "-import(eosstd).\n",  // import,defineは、自分で処理しなければならない
            HdSrc = "",
            erleos_emit:invoke(Emitter,compile,[HdSrc,ModifiedSrc])

        end
    ).

compile(Module,Src,Options)->
    erleos:try_block( fun()->
        {ok,compile_(Module,Src,Options)}
    end,
    "compile"
    ).

compile(Module,Src)->
    compile(Module,Src,[]).


translate_block(Src)->
    {ok,A} = try_block(
        fun()->
            {ok,erleos_lexer:lex(Src)}
        end
        ,"lexer"
    ),
    Block = use( erleos_parser:start(),
        fun(Parser)->
            erleos_parser:invoke(Parser,init,[A]),
            erleos_parser:invoke(Parser,parse_block,[<<"translate block">>])
        end
    ),
    use( erleos_emit:start(),
        fun(Emitter)->
            erleos_emit:invoke(Emitter,init,[fun(S)->[] end]),
            erleos_emit:invoke(Emitter,exec_emit_block,["",Block])
        end
    ).


path_to_module(Path)->
    to_atom( filename:basename(Path,".eos") ).

load(Filename)->
    io:format("# filename = ~s\n",[Filename]),
    {ok,Binary} = file:read_file( Filename ),
    unicode:characters_to_list(Binary,utf8).

trim_dir(Dir)->
    case lists:last(Dir) of
        $/ -> Dir;
        _ -> Dir ++ "/"
    end.

compile_dir(Dir_,DstDir_)->
    Dir = trim_dir(Dir_),
    DstDir = trim_dir(DstDir_),
    io:format("# compile dir: ~p -> ~p\n",[Dir,DstDir]),
    {ok,Files} = file:list_dir(Dir),
    Targets = lists:foldl(
        fun(File,Acc)->
            case filename:extension(File) of
                ".eos" -> [File|Acc];
                _ -> Acc
            end
        end,
        [],
        Files),

    lists:foreach(
        fun(File)->
            SrcPath = eosstd:fmt("~s~s",[Dir,File] ),
            Module = erleos:path_to_module(SrcPath),
            DstPath = eosstd:fmt("~s~s.erl",[DstDir,Module] ),
            compile_file(SrcPath,DstPath)
        end,
        Targets
    ).

compile_dir(Dir)->
    compile_dir(Dir,Dir).

compile_file(SrcPath)->
    Module = erleos:path_to_module(SrcPath),
    DstDir = filename:dirname(SrcPath)++"/",
    DstPath = eosstd:fmt("~s~s.erl",[DstDir,Module] ),
    compile_file(SrcPath,DstPath).

compile_file(SrcPath,DstPath)->
    Module = erleos:path_to_module(SrcPath),
    %io:format("~s : ~s --------------------------------------------\n",[Module,Module]),
    Src = load(SrcPath),
    case erleos:compile(Module,Src) of
        {ok,ErlSrc} ->
            %io:format("SRC = ~p\n",[ErlSrc]),
            file:write_file(DstPath,eosstd:to_bin(ErlSrc));
        _ -> []
    end.

erlsrc_to_binary(CompiledSrc)->
    StrForms = CompiledSrc,

    %io:format("\nconvert to forms\n"),
    Forms = lists:map(
        fun(StrForm)->
            %io:format("FORM = ~p\n",[StrForm]),
            erleos:try_block( fun()->
                {ok,Tokens,_} = erl_scan:string(StrForm),
                {ok,Form} = erl_parse:parse_form(Tokens),
                Form
            end,
            "erlang source to binary"
            )

        end,
        StrForms ),
    %io:format("compile forms\n"),
    compile:forms(Forms).

erlsrc_to_module(CompiledSrc)->
    {ok,ModuleName,Binary} = erlsrc_to_binary(CompiledSrc),
    code:load_binary(ModuleName, "nofile", Binary).


load_eos_module(SrcPath)->
    Module = erleos:path_to_module(SrcPath),
    Src = load(SrcPath),
    case erleos:compile(Module,Src) of
        {ok,ErlSrc} ->
            erlsrc_to_module(ErlSrc);
        _ -> []
    end.
