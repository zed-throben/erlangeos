-module(eos@etsobj).
-compile(export_all).
-include("eos.hrl").

new(Options,Params)->
    Obj = new(),
    lists:foreach(
        fun({Key,Value})->
            set_slot(Obj,Key,Value) end,
        Params),
    Obj.

ets_pid(?eos(eos@etsobj,PID))->
    PID.

new()->
    PID = ets:new(undefined,[set,public]),
    ?eos(eos@etsobj,PID).

clone(?eos(eos@etsobj,PID)=Obj)->
    NewObj = new(),
    eos_etsutil:clone(PID,ets_pid(NewObj)),
    NewObj.
    
set_slot(?eos(eos@etsobj,PID),Key,Value)->
    ets:insert(PID,{Key,Value}),
    Value.

get_slot(?eos(eos@etsobj,PID),Key)->
    case ets:lookup(PID,Key) of
        [{Key,Value}] -> Value;
        _ -> undefined
    end.

modify(?eos(eos@etsobj,PID)=Obj,Key,Fn)->
    NewVal = Fn( get_slot(Obj,Key) ),
    set_slot(Obj,Key,NewVal),
    NewVal.

invoke(?eos(eos@etsobj,PID)=Obj,'clone',[])->
    clone(Obj);

invoke(?eos(eos@etsobj,PID)=Obj,Member,Param)->
    case get_slot(Obj,Member) of
        undefined ->
            case get_slot(Obj,missing_method) of
                undefined -> erlang:error({missing_method,Member});
                Fn -> Fn(Obj,Param)
            end;
        Fn -> Fn(Obj,Param)
    end.
