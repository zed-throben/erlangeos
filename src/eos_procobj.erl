-module(eos_procobj).
-compile(export_all).
-include("eos_procobj.hrl").

new()->
	start().

new(Dic)->
	Obj = start(),
	set_dict(Obj,Dic),
	Obj.
	
start()->
    start( ?MODULE,fun()->[]end,false ).

start_link()->
    start( ?MODULE,fun()->[]end,true ).


invoke_(Obj,Method,Param)->
	F = erlang:get(Method),
	erlang:apply(F,Param).