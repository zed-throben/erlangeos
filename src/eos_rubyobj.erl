- compile(export_all).
- module(eos_rubyobj).
- compile(export_all).
- include("eos.hrl").


invoke(?eos(eos_rubyobj,{Module,PID}) = Obj,Member,Param) ->
    ruby:call(PID,Module,Member,Param).
