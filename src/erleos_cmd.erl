-module(erleos_cmd).
-compile(export_all).

main()->
	%io:format("no arguments.\n").
	erleos_repl:start().

rebar()->
	Rebar = case os:cmd("which rebar") of
		[] -> "./rebar";
		_  -> "rebar"
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
			erleos:compile_dir("./src/"),
			Res = os:cmd( rebar()++" compile"),
			io:format("~s\n",[Res]),
			erlang:halt();

		%
		["new",ID] ->
			Res = os:cmd( rebar()++" create-app appid="++ID),
			io:format("~s\n",[Res]),
			erlang:halt();

		["run"] ->
			run(false);

		["dbg"] ->
			run(true);

		[Path] ->
			io:format("run directory: ~p\n",[Path])
	end.

config()->
	case file:consult(<<"eos.config">>) of
		{ok,Config} ->
			Config;
		_ -> []
	end.

code_add_path(Path)->
	io:format("ebin: ~p\n",[Path]),
	code:add_path(Path).

apply_config()->
	C = config(),
	case proplists:lookup(ebin,C) of
		{ebin,Dirs} ->
			lists:foreach(
				fun(Dir)->code_add_path(Dir) end,
				Dirs
			);
		_ -> undefined
	end.

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

	apply_config(),
	code:add_path("ebin"),
	application:start( eosstd:to_atom(App) ),


	if WithRepl ->
			erleos_repl:start();
		true ->
			erlang:halt()
	end.

eval(WithRepl,Module,Fun,Params)->
	apply_config(),
	code:add_path("ebin"),

	erlang:apply(Module,Fun,Params),

	if WithRepl ->
			erleos_repl:start();
		true ->
			erlang:halt()
	end.

