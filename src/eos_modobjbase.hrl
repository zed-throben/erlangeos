-export([addref/1,release/1,start/0,invoke/3,get_slot/2,set_slot/3,lpush/3,lpop/2,lunshift/3,lshift/2]).
-include("eos_var.hrl").
-include("eos.hrl").
-include("erleos.hrl").
-define(REFCOUNT,'_refcount').

msgproc(Module)->
	receive
		{From,set_module,NewModule}->
			From ! {result,ok},
			NewModule;

		{From,invoke,Method,Param} ->
			%io:format("loop:invoke ~p ~p ~p\n",[Module,Method,Param]),
			%Res = erlang:apply(Module,Method,Param),

			%
			%
			%
%			ResMsg = try
%				?invoke(Module,Method,Param)
%				%io:format("  -> ~p\n",[Res]),
%			catch
%				error:X -> {error,X};
%				throw:X -> {throw,X};
%				exit:X -> {exit,X}
%			end,
			
			%
			%
			%
			ResMsg = 
				if ?EOSRELEASE ->
						try
							{ok,?invoke(Module,Method,Param)}
						catch
							EType:EMsg ->
								{exception,EType,EMsg}
						end;
					true ->
						{ok,?invoke(Module,Method,Param)}
				end,

			From ! {result,ResMsg},
			Module;

		{From,get,Key} ->
			From ! {result,erlang:get(Key)},
			Module;

		{From,put,Key,Value} ->
			From ! {result,erlang:put(Key,Value)},
			Module;

		{From,set_dict,Dic} ->
			lists:foreach(
				fun({K,V})->
					put(K,V)
				end,
				Dic
			),
			From ! {result,ok},
			Module;

		{From,addref} ->
			NewCount = erlang:get(?REFCOUNT)+1,
			erlang:put(?REFCOUNT,NewCount),
			From ! {result,NewCount},
			Module;

		{From,release} ->
			NewCount = erlang:get(?REFCOUNT)-1,
			erlang:put(?REFCOUNT,NewCount),
			From ! {result,NewCount},
			if NewCount =/= 0 -> Module;
				true ->
					%io:format("@exit process\n"),
					[]
			end;

		quit ->
			io:format("@quit process\n"),
			[];

		%{result,X}->
		%	X;

		X ->
			io:format("invalid message: ~p\n",[X]),
			Module
	end.

loop([])->
	[];
loop(Module)->
	loop( msgproc(Module) ).


%

start(Module,InitFun,Link)->
	Proc =
		fun()->
			put(?REFCOUNT,0),
			InitFun(),
			loop(Module)
		end,
	if  Link -> start_     (Module,Proc);
		true -> start_link_(Module,Proc)
	end.

send_and_receive( ?eos(?OBJTYPE,{Module,Pid})=Obj,Message )->
	%io:format("send ~p\n",[Message]),
	Pid ! Message,
	Res = if Pid =:= self() ->
		%io:format("**self\n"),
		msgproc(Module),
		receive {result,X} -> X end;
	true ->
		%io:format("**send\n"),
		receive {result,X} -> X end
	end,
	%io:format("res = ~p\n",[Res]),
	Res.


start_link_(Module,F)->
	Pid = spawn_link(F),
	?eos(?OBJTYPE,{Module,Pid}).

start_(Module,F)->
	Pid = spawn_link(F),
	?eos(?OBJTYPE,{Module,Pid}).


invoke(?eos(?OBJTYPE,{Module,Pid})=Obj,Method,Param)->
	%io:format("invoke ~p ~p ~p\n",[Obj,Method,Param]),
	case send_and_receive( Obj,{self(),invoke,Method,Param} ) of
		{ok,Res} -> Res;
		{exception,EType,EMsg} ->
			case EType of
				error -> error(EMsg);
				exit  -> exit(EMsg);
				throw -> throw(EMsg)
			end
	end.


get_slot(?eos(?OBJTYPE,{Module,Pid})=Obj,Key)->
	send_and_receive( Obj,{self(),get,Key} ).

set_slot(?eos(?OBJTYPE,{Module,Pid})=Obj,Key,Value)->
	send_and_receive( Obj,{self(),put,Key,Value} ).


%
%

set_dict(?eos(?OBJTYPE,{Module,Pid})=Obj,Dict)->
	send_and_receive( Obj,{self(),set_dict,Dict} ).

lpush(?eos(?OBJTYPE,{Module,Pid})=Obj,Key,Value)->
	send_and_receive( Obj,{self(),lpush,Key,Value} ).

lpop(?eos(?OBJTYPE,{Module,Pid})=Obj,Key)->
	send_and_receive( Obj,{self(),lpop,Key} ).

lunshift(?eos(?OBJTYPE,{Module,Pid})=Obj,Key,Value)->
	send_and_receive( Obj,{self(),put,lunshift,Key,Value} ).

lshift(?eos(?OBJTYPE,{Module,Pid})=Obj,Key)->
	send_and_receive( Obj,{self(),put,lshift,Key} ).

%

addref(?eos(?OBJTYPE,{Module,Pid})=Obj)->
	send_and_receive( Obj,{self(),addref} ).

release(?eos(?OBJTYPE,{Module,Pid})=Obj)->
	send_and_receive( Obj,{self(),release} ).


