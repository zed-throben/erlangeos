-module(eos@js).
-compile(export_all).
-include("eos.hrl").

new(ClassOpts,Options)->
	{ok, JS} = js_driver:new(),
	?eos(eos@js,{JS,[]}).

get_slot( ?eos(eos@js,{JS,Path} ),Key ) ->
	?eos(eos@js,{JS,Path++[Key]}).

set_slot( ?eos(eos@js,{JS,[]} ),Key,Value ) ->
	js:define(JS, eosstd:to_bin( eosstd:fmt("var ~s = ~s",[Key,eosstd:to_str(Value)]) ) ).

invoke( ?eos(eos@js,{JS,[]} ),'eval',[Param] ) ->
	%io:format("eval ~s\n",[Param]),
	{ok,Res} = js:eval(JS,eosstd:to_bin(Param)),
	Res;

invoke( ?eos(eos@js,{JS,[]} ),Method,Params ) ->
	%io:format("call ~s ~p\n",[Method,Params]),
	{ok,Res} = js:call(JS, eosstd:to_bin(Method), Params),
	Res.
