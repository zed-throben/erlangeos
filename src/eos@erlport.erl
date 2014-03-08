-module(eos@erlport).
-compile(export_all).
-include("eos.hrl").

new(ClassOpts,Options)->
	[LangType|_] = ClassOpts,
	{ok,PID} =
		case LangType of
			python -> python:start(Options);
			ruby -> ruby:start(Options)
		end,
	?eos(eos@erlport,{LangType,PID,[]}).

get_slot( ?eos(eos@erlport,{LangType,PID,Path} ),Key ) ->
	?eos(eos@erlport,{LangType,PID,Path++[Key]}).

invoke( ?eos(eos@erlport,{LangType,PID,Path} ),Method,Params ) ->
	%Params = lists:map( fun(X)->eosstd:to_bin(eosstd:to_str(X)) end,Params_),
	%io:format("invoke ~p , ~p , ~p\n",[Path,Method,Params]),
	case LangType of
		python -> python:call(PID,python_path(Path),Method,Params );
		ruby   -> ruby  :call(PID,ruby_path  (Path),Method,Params )
	end.

python_path(Path)->
	Path2 = lists:map(fun(X)->eosstd:to_str(X) end,Path),
	list_to_atom( string:join(Path2,".") ).

ruby_path(Path)->
	Path2 = lists:map(fun(X)->eosstd:to_str(X) end,Path),
	list_to_atom(string:join(Path2,"/") ).
