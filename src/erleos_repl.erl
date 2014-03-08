-module(erleos_repl).
-compile(export_all).
-include("erleos.hrl").

-record(env,{bindings,verbose,rescue=?EOSRELEASE}).
-define(env(Bindings),#env{bindings=Bindings} ). 

trim(X)->
	string:strip(X,both,$\n).

initial_message()->
	lists:flatten(
		io_lib:format("ErlangEOS ~s (abort with ^G)",[?VERSION])++
	    if ?EOSRELEASE /= true -> " ** DEBUG MODE **";
	    	true -> []
	    end
	). 

init()->
	Bindings = erl_eval:bindings(erl_eval:new_bindings()),
	?env(Bindings).

start()->
    io:fwrite( initial_message() ),
    io:fwrite("\n"),
	loop( init() ).


loop(Env)->
	Line = io:get_line("eos> "),
	case eval_line(Env,Line) of
		'exit' -> [];
		'clear' -> start();
		{ok,NewEnv,Ret} ->
			io:format("=> ~p\n\n",[Ret]),
			loop(NewEnv)
	end.

eval_line(Env,LineBin)->
	Line = eosstd:to_list(LineBin),	
	Token = string:tokens(trim(Line)," \t"),
	%io:format("token = ~p\n",[Token]),
	case Token of
		[":exit"] ->
			'exit';
		[":clear"] ->
			'clear';

		[":verbose"]->
			{ok,Env#env{verbose=true} };

		[":quiet"]->
			{ok,Env#env{verbose=false} };

		[":help"] ->
			{ok,Env,
				":exit :clear :help\n"};

		[":load",Filename]->
			try
				erleos:load_source_and_compile(Filename)
			catch
				Ex ->
					io:format("load failed: ~p , ~p\n",[Filename,Ex])
			end,
			{ok,Env,ok};

		_ ->
			{Ret,NewEnv} = 
				if Env#env.rescue ->
					try
						eval_eos(Env,Line)
					catch
						_:X -> {eosstd:fmt("error: ~p\n",[X]),Env}
					end;

					true ->
						eval_eos(Env,Line)
				end,

			{ok,NewEnv,Ret }
	end.

eval_eos(Env,Line)->
	Src = erleos:translate_block(Line) ++ ".",

	if Env#env.verbose ->
		io:format("\n"),
		io:format("~s\n",[Src]);
		true -> []
	end,

	{Ret,NewEnv} = eval_erl(Env,Src).

eval_erl(Env,Expr)->
 	{ok, Tokens, _}=erl_scan:string(Expr),
 	%{ok,[Expression]} = erl_parse:parse_exprs(Tokens),
 	%{value, Ret, _} = erl_eval:expr(Expression ,erl_eval:bindings(erl_eval:new_bindings())),
 	%{value, Ret, _} = lists:foreach( fun(Expression)->erl_eval:expr(Expression ,Bindings) end,Expressions ),

 	case erl_parse:parse_exprs(Tokens) of
 	{ok,Expressions} ->
	 	lists:foldl(
	 		fun(Expression,{ARet,AEnv})->
	 			%io:format("expr: ~p\n",[Expression]),
	 			case erl_eval:expr(Expression,AEnv#env.bindings) of
	 				{value, Ret, NewBindings} -> {Ret,Env#env{bindings=NewBindings} };
	 				X ->
	 					%io:format("intermediate: ~p\n",[X]),
	 					{ARet,AEnv}
	 			end
	 		end,
	 		{[],Env},
	 		Expressions );
	 X ->
	 	io:format("error: ~p\n",[X]),
	 	{error,Env}
	 end.
