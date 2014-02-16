-define(OBJTYPE,'eos_modobj').
-define(invoke(Module,Method,Param),erlang:apply(Module,Method,Param) ).
-include("eos_modobjbase.hrl").

start()->
    start( ?MODULE,fun init_module_object/0,false ).

start_link()->
    start( ?MODULE,fun init_module_object/0,true ).
