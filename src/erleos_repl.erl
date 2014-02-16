-module(erleos_repl).
-compile(export_all).
-include("erleos.hrl").

-record(env,{bindings,verbose,rescue=?EOSRELEASE}).
-define(env(Bindings),#env{bindings=Bindings} ). 

trim(X)->
	string:strip(X,both,$\n).

start()->
    io:fwrite("ErlangEOS ~s (abort with ^G)\n",[?VERSION]),

	Bindings = erl_eval:bindings(erl_eval:new_bindings()),
	loop( ?env(Bindings) ).


loop(Env)->
	Line = io:get_line("eos> "),

	case trim(Line) of
		":exit" ->
			ok;
		":clear" ->
			start();

		":verbose"->
			loop( Env#env{verbose=true} );

		":quiet"->
			loop( Env#env{verbose=false} );

		":help" ->
			io:format(":exit :clear :help\n"),
			loop( Env );

		_ ->
			{Ret,NewEnv} = 
				if Env#env.rescue ->
					try
						eval(Env,Line)
					catch
						_:X -> {eosstd:fmt("error: ~p\n",[X]),Env}
					end;

					true ->
						eval(Env,Line)
				end,

			io:format("=> ~p\n\n",[Ret]),
			loop( NewEnv )
	end.

eval(Env,Line)->
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
