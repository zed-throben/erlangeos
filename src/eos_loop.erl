-module(eos_loop).
-compile(export_all).
%%-export([break/1,continue/0,times/2,loop/1]).

break(X)->
	erlang:throw({eos,{break,X}}).

continue()->
	erlang:throw({eos,continue}).

%
times_(F,0)->
	undefined;

times_(F,1)->
	try
		F()
	catch
		throw:{eos,continue} -> undefined
	end;

times_(F,Times)->
	try
		F()
	catch
		throw:{eos,continue} -> undefined
	end,
	times_(F,Times-1).
%
times_1(F,Max,Max)->
	try
		F(Max)
	catch
		throw:{eos,continue} -> undefined
	end;

times_1(F,Max,Count)->
	try
		F(Count)
	catch
		throw:{eos,continue} -> undefined
	end,
	times_1(F,Max,Count+1).

%
times(F,Times)->
	try
		FunInfo = erlang:fun_info(F),
		case proplists:get_value(arity,FunInfo) of
			0 -> times_(F,Times);
			1 -> times_1(F,Times,1)
		end
	catch
		throw:{eos,{break,X}} -> X
	end.

%

while(FBody,FCond)->
	try
		while_2(FBody,FCond,undefined)
	catch
		throw:{eos,{break,X}} -> X
	end.


while_2(FBody,FCond,Res)->
	case FCond() of
		true ->
			try
				while_2(FBody,FCond,FBody())
			catch
				throw:{eos,continue} ->
					while_2(FBody,FCond,Res)
			end;

		false ->
			Res
	end.

loop(FBody)->
	try
		loop_1(FBody)
	catch
		throw:{eos,{break,X}} -> X
	end.

loop_1(FBody)->
	Res = try
		FBody()
	catch
		throw:{eos,continue} -> true
	end,

	%case Res of
	%	true -> loop_1(FBody);
	%	_ -> undefined
	%end.

	loop_1(FBody).