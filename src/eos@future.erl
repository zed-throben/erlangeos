-module(eos@future).
-compile(export_all).
-include("eos.hrl").

%get_slot(?eos(eos@future,ID),Key)->
%	case erlang:get({future,ID}) of
%		undefined ->
%			receive
%				{eos@future,ID,Value} ->
%					erlang:put({future,ID},Value),
%					eos:get_slot(Value,Key)
%			end;
%
%		Value ->
%			eos:get_slot(Value,Key)
%	end.
%
%start(F)->
%	MyPID = self(),
%	ID = erleos_gensym:newid(),
%	spawn(
%		fun()->
%			try
%				Res = F(),
%				MyPID ! {eos@future,ID,Res}
%			catch
%				Et:Ex ->
%					MyPID ! {eos@future,ID,[{exception_class,Et},{exception,Ex}] }
%			end
%
%		end
%	),
%	?eos(eos@future,ID).

start(F)->
	MyPID = self(),
	ID = erleos_gensym:newid(),
	spawn(
		fun()->
			try
				Res = F(),
				MyPID ! {eos@future,ID,Res}
			catch
				Et:Ex ->
					MyPID ! {eos@future,ID,[{exception_class,Et},{exception,Ex}] }
			end

		end
	),
	fun()->
		receive
			{eos@future,ID,Value} ->
				Value
		end
	end.
