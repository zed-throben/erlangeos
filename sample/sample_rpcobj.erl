- module('sample_rpcobj').
- compile(export_all).


start() ->
    RPC1 = eos:new(eos@rpc,[],[{node , '192.168.0.2@main' },{module , main }]),
    eos:invoke(RPC1,hello,["world"]),
    RPC2 = eos:new(eos@rpc,[],[{node , '192.168.0.3@main' },{module , main }]),
    eos:invoke(RPC2,hello,["world"]).
