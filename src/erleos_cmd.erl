-module(erleos_cmd).
-compile(export_all).

vprint(Fmt,Param)->
	Verbose = get(verbose),
	if Verbose == true -> io:format(Fmt,Param);
		true -> []
	end.

vprint(Fmt)->
	Verbose = get(verbose),
	if Verbose == true -> io:format(Fmt);
		true -> []
	end.

init()->
	erleos:start(),
	apply_config(),
	addLibraries().


main()->
	repl().

rebar()->
	case os:cmd("which rebar") of
		[] ->
			Rebar = "./rebar",
			case filelib:is_file(Rebar) of
				true -> Rebar;
				false ->
					io:format("can't find 'rebar'.\n"),
					erlang:halt()
			end;

		_  ->
			"rebar"
	end.

main(CmdLine)->

    %io:format("CmdLine = ~p\n",[CmdLine]),
	case CmdLine of
		[":test"] ->
			erleos_selftest:all();

		["auto"] ->
			erleos_auto_compiler:start("./src/");

		["auto",Dir] ->
			erleos_auto_compiler:start(Dir);

		["auto",SrcDir,DstDir] ->
			erleos_auto_compiler:start(SrcDir,DstDir);

		["clean"] ->
			os:cmd("rm -f ./ebin/*.beam"),
			SrcDir = "./src/",
			{ok,Files} = file:list_dir(SrcDir),
			EosFiles = lists:filter(
				fun(F)-> filename:extension(F) == ".eos" end,
				Files
			),
			ErlFiles = lists:map(
				fun(F)-> filename:absname( SrcDir++filename:rootname(F)++".erl" ) end,
				EosFiles
			),
			%io:format("FILES = ~p\n",[ErlFiles]),
			lists:foreach(
				fun(F)->
					vprint("delete ~s\n",[F]),
					file:delete(F)
				end,
				ErlFiles
			),
			erlang:halt();

		%
		["c"] ->
			erleos:compile_dir("./src/"),
			erlang:halt();

		["c",Dir] ->
			erleos:compile_dir(Dir),
			erlang:halt();

		["c",SrcDir,DstDir] ->
			erleos:compile_dir(SrcDir,DstDir),
			erlang:halt();

		%
		["b"] ->
			Res = build(),
			io:format("~s\n",[Res]),
			erlang:halt();

		%
		["new",ID] ->
			Res = os:cmd( rebar()++" create-app appid="++ID),
			io:format("~s\n",[Res]),
			erlang:halt();

		["run"] ->
			Res = build(),
			case string:str(Res,"rebar abort\n") of
				0 -> run(false);
				_ ->
					io:format("~s\n",[Res]),
					erlang:halt()
			end;


		["dbg"] ->
			run(true);

		[Path] ->
			io:format("run directory: ~p\n",[Path])
	end.

build()->
	erleos:compile_dir("./src/"),
	Res = os:cmd( rebar()++" compile").

config()->
	case file:consult(<<"eos.config">>) of
		{ok,Config} ->
			Config;
		_ -> []
	end.

apply_config()->
	C = config(),
	case proplists:lookup(path,C) of
		{path,Dirs} ->
			lists:foreach(
				fun(Dir)->lib_path(Dir) end,
				Dirs
			);			
		_ -> undefined
	end,
	case proplists:lookup(env,C) of
		{env,KeyVals} ->
			lists:foreach(
				fun({K,V})->
					io:format("Env ~p = ~p\n",[K,V]),
					os:putenv( eosstd:to_str(K),eosstd:to_str(V) )
				end,
				KeyVals
			);			
		_ -> undefined
	end,
	true.	

run(WithRepl)->
	{ok,Files} = file:list_dir("ebin"),
	[AppFile|_] = lists:filter(
		fun(F)-> filename:extension(F) == ".app" end,
		Files
	),
	App = filename:rootname(AppFile),
	%Cmdline = "erl -pa ebin -eval \"application:start("++App++")\"",
	%%io:format("CmdLine = ~s",[Cmdline]),
	%Res = os:cmd(Cmdline),
	%io:format("~s\n",[Res]),

	%io:format("App = ~s\n",[App]),

	init(),
	application:start( eosstd:to_atom(App) ),


	if WithRepl ->
			erleos_repl:start();
		true ->
			%%erlang:halt()
			[]
	end.

eval(WithRepl,Module,Fun,Params)->
	init(),

	erlang:apply(Module,Fun,Params),

	if WithRepl ->
			erleos_repl:start();
		true ->
			erlang:halt()
	end.

repl()->
	%io:format("no arguments.\n").
	init(),
	erleos_repl:start().


lib_path(X)->
	Absname = filename:absname(X),
	vprint("library path: ~s(~s)\n",[X,Absname]),
	code:add_path(Absname).

addLibraries()->
	%code:add_path("ebin"),
	lib_path("./ebin"),

	Base = "./deps/",
	addLib(Base).

addLib(Base)->
	case file:list_dir(Base) of
		{ok,Files} -> 
			lists:foreach(
				fun(F)->
					RPath = eosstd:fmt("~s~s/ebin/",[Base,F]),
					%APath = filename:absname(RPath),
					%io:format("deps path = ~s(~s)\n",[RPath,APath])
					lib_path(RPath),

					RPath2 = eosstd:fmt("~s~s/deps/",[Base,F]),
					addLib(RPath2)
				end,
				Files);

		_ ->
			vprint("no deps libraries\n")
	end.
