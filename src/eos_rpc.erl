-module(eos_rpc).
-compile(export_all).
-include("eos.hrl").

new(Options,Params)->
	{node,Node} = proplists:lookup(node,Params),
	{module,Module} = proplists:lookup(module,Params),
	?eos(eos_rpc,{Node,Module}).

invoke(?eos(eos_rpc,{Node,Module}),Method,Params)->
    rpc:call(Node,Module,Method,Params).

