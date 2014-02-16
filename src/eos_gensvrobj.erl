-module(eos_gensvrobj).
-compile(export_all).
-include("eos.hrl").

invoke(?eos(eos_gensvrobj,Pid),Method,Params)->
    gen_server:call(Pid,{Method,Params}).

