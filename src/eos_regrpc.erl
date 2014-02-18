-module(eos_regrpc).
-compile(export_all).
-include("eos.hrl").

start()->
	ets:new(registered_nodes,[public,named_table,set]).

register(Name,Address)->
	ets:insert(registered_nodes,{Name,Address}).

invoke(?eos(eos_regrpc,{Node,Module}),Method,Params)->
	[{Node,Address}] = ets:lookup(registered_nodes,Node),
    rpc:call(Address,Module,Method,Params);

invoke(?eos(eos_regrpc,{Node,Module,Timeout}),Method,Params)->
	[{Node,Address}] = ets:lookup(registered_nodes,Node),
    rpc:call(Address,Module,Method,Params,Timeout).
