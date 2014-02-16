-module(eos_rpcobj).
-compile(export_all).
-include("eos.hrl").

create(Params)->
	{node,Node} = proplists:lookup(node,Params),
	{module,Module} = proplists:lookup(module,Params),
	?eos(eos_rpcobj,{Node,Module}).

invoke(?eos(eos_rpcobj,{Node,Module}),Method,Params)->
    rpc:call(Node,Module,Method,Params).

