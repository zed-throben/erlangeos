- module('erleos_auto_compiler').
- compile(export_all).
- include_lib("kernel/include/file.hrl").

compile_dir(Dir) ->
    compile_dir(Dir,Dir).

compile_dir(Dir,DstDir) ->
    ( fun(Dir ,DstDir ) -> 
        {ok,Files} = file:list_dir(Dir),
        Targets = lists:foreach(fun (File) -> 
            
            (fun()->EOSSYS@t_0 = filename:extension(File) == ".eos" ,
            if
                EOSSYS@t_0 ->
                    procfile(Dir,DstDir,File);
                true -> []
            end end)() end,Files)
    end)(erleos:trim_dir(Dir) ,erleos:trim_dir(DstDir) ).

change_ext(Source,Ext) ->
    filename:rootname(Source) ++ Ext .

procfile(Dir,DstDir,File) ->
    Src = eosstd:fmt("~s~s",[eosstd:to_str(Dir),eosstd:to_str(File)]),
    ErlFile = change_ext(File,".erl"),
    Dst = eosstd:fmt("~s~s",[eosstd:to_str(DstDir),eosstd:to_str(ErlFile)]),
    case {file:read_file_info(Src),file:read_file_info(Dst)} of
        { { ok ,  FIA } ,  { ok ,  FIB } }  -> 
            Diff = calendar:datetime_to_gregorian_seconds((FIA#file_info.ctime)) - calendar:datetime_to_gregorian_seconds((FIB#file_info.ctime)) ,
            
            (fun()->EOSSYS@t_1 = Diff > 0 ,
            if
                EOSSYS@t_1 ->
                    compile(Dir,DstDir,File);
                true -> []
            end end)();
        _  -> 
            []
        
    end.

compile(Dir,DstDir,File) ->
    Src = eosstd:fmt("~s~s",[eosstd:to_str(Dir),eosstd:to_str(File)]),
    ErlFile = change_ext(File,".erl"),
    Dst = eosstd:fmt("~s~s",[eosstd:to_str(DstDir),eosstd:to_str(ErlFile)]),
    eosstd:puts(eosstd:fmt("compile ~s -> ~s",[eosstd:to_str(Src),eosstd:to_str(Dst)])),
    erleos:compile_file(Src,Dst).

loop(Dir,DstDir) ->
    compile_dir(Dir,DstDir),
    timer:sleep(2500),
    loop(Dir,DstDir).

start(Dir,DstDir) ->
    spawn(fun () -> 
        loop(Dir,DstDir) end).

start(Dir) ->
    start(Dir,Dir).
