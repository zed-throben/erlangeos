-module(erleos_selftest).
-compile(export_all).

%
%
%

f(F,P)->
    eosstd:lsformat(F,P).

to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_list(X) -> X.

p(X)->
    io:format("~s~n",[X]).

pp(X)->
    io:format("~p~n",[X]).

%
%
%


src(N)->
    Filename = lists:flatten( io_lib:format("../src/test~w.eos",[N]) ),
    {ok,Binary} = file:read_file( Filename ),
    unicode:characters_to_list(Binary,utf8).

load(F)->
    BaseDir = "../test/",
    Filename = lists:flatten( io_lib:format("~s~s",[BaseDir,F]) ),
    {ok,Binary} = file:read_file( Filename ),
    unicode:characters_to_list(Binary,utf8).

%

all()->
    {ok,Files} = file:list_dir("../test/"),
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
            Module = erleos:path_to_module(File),
            io:format("\n\n~s : ~s --------------------------------------------\n",[Module,File]),
            Src = load(File),
            case erleos:compile(Module,Src) of
                {ok,ErlSrc} ->
                    erleos:erlsrc_to_module(ErlSrc),
                    erlang:apply(Module,test,[]);

                _ ->
                    io:format("## compile error! #######################################\n")
            end
        end,
        Targets
    ).



t()->
    Src = src(0),
    erleos:compile("test",Src,[verbose]).

t(X)->
    Src = src(X),
    erleos:compile("",Src).




