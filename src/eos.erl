-module(eos).
-compile(export_all).
-include("eos.hrl").

invoke(Obj,Param) when is_function(Obj) ->
    erlang:apply(Obj,Param).

invoke(?eos(eos_modobj,{Module,Obj})=EOBJ,Method,Param) ->
    erlang:apply(Module,invoke,[EOBJ,Method,Param]);

invoke(?eos(Type,TypeParam)=Obj,Method,Param) ->
    erlang:apply(Type,invoke,[Obj,Method,Param]);

invoke(Obj,Method,Param) when is_list(Obj)->
    invoke_list(Obj,Method,Param);

invoke(?DICTIONARY=Obj,Method,Param) ->
    invoke_dict(Obj,Method,Param);

invoke(Obj,Method,Param) when is_binary(Obj)->
    invoke_binary(Obj,Method,Param);

invoke(Obj,Method,Param) when is_pid(Obj)->
    invoke_pid(Obj,Method,Param).

%

invoke_list(Obj,Method,Param)->
	%io:format("invoke list: ~p ~p ~p\n",[Obj,Method,Param]),
    erlang:apply(eos@list,Method,[Obj|Param]).

invoke_dict(Obj,Method,Param)->
    %io:format("invoke dict: ~p ~p ~p\n",[Obj,Method,Param]),
    erlang:apply(eos@dict,Method,[Obj|Param]).

invoke_binary(Obj,Method,Param)->
    erlang:apply(eos@bin,Method,[Obj|Param]).

invoke_pid(Obj,Method,Param)->
    rpc:call(Obj,Method,[Param]).

%

addref(?eos(eos_modobj,{Module,Obj})=EOBJ) ->
    erlang:apply(Module,addref,[EOBJ]);
addref(?eos(Type,TypeParam)=EOBJ) ->
    erlang:apply(Type,addref,[EOBJ]).

release(?eos(eos_modobj,{Module,Obj})=EOBJ) ->
    erlang:apply(Module,release,[EOBJ]);
release(?eos(Type,TypeParam)=EOBJ) ->
    erlang:apply(Type,release,[EOBJ]).

%

monad(Module) when is_atom(Module)->
    erlang:apply(Module,start,[]).

using(Obj,F)->
    addref(Obj),
    R = F(),
    release(Obj),
    R.

%
set_slot(?eos(Type,TypeParam)=Obj,Key,Value)->
    erlang:apply(Type,set_slot,[Obj,Key,Value]);

set_slot(?DICTIONARY=Obj,Key,Value) ->
    eos@dict:set_slot(Obj,Key,Value);

set_slot(Obj,Key,Value) when is_list(Obj)->
    eos@list:set_slot(Obj,Key,Value).

%
get_slot(?eos(Type,TypeParam)=Obj,Key)->
    erlang:apply(Type,get_slot,[Obj,Key]);

get_slot(?DICTIONARY=Obj,Key) ->
    eos@dict:get_slot(Obj,Key);

get_slot(Obj,Key) when is_list(Obj)->
    eos@list:get_slot(Obj,Key).

%
new(Type,Option,Params)->
    %io:format("#eos:new ~p ~p ~p\n",[Type,Option,Params]),
    erlang:apply(Type,new,[Option,Params]).
