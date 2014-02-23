-module(eos@obj).
-compile(export_all).
-include("eos.hrl").

new(Option,Params)->
	Pid = spawn( fun loop/0 ),
	Obj = ?eos(eos@obj,Pid),

	%io:format("eos@obj:new ~p\n",[Params]),
    lists:foreach(
        fun(Item)->
			%io:format("* ~p\n",[Item]),
        	{Key,Value} = Item,
            set_slot(Obj,Key,Value) end,
        Params),
   	Obj.

loop()->
	%io:format("loop: ~p\n",[self()]),
	receive
		{Sender,get_all}->
			Sender ! {eos@obj,get_all,ok,get()};

		{Sender,get_slot,Key}->
			Sender ! {eos@obj,get_slot,ok,get(Key)};

		{Sender,set_slot,Key,Value}->
			put(Key,Value);

		X ->
			io:format("invalid message:A ~p\n",[X])
	end,
	loop().

get_all(?eos(eos@obj,Pid))->
	%io:format("get_slot: ~p\n",[self()]),
	Pid ! {self(),get_all},
	receive
		{eos@obj,get_all,ok,KeyValues} -> KeyValues
	end.

get_slot(?eos(eos@obj,Pid),Key)->
	%io:format("get_slot: ~p\n",[self()]),
	Pid ! {self(),get_slot,Key},
	receive
		{eos@obj,get_slot,ok,Value} -> Value
	end.

set_slot(?eos(eos@obj,Pid),Key,Value)->
	%io:format("set_slot: ~p\n",[self()]),
	Pid ! {self(),set_slot,Key,Value},
	Value.

clone(?eos(eos@obj,Pid)=Obj)->
	KeyValues = get_all(Obj),
	new([],KeyValues).

invoke(Obj,'clone',[])->
	clone(Obj);

invoke(Obj,Method,Param)->
    case get_slot(Obj,Method) of
        undefined ->
            case get_slot(Obj,missing_method) of
                undefined -> erlang:error({missing_method,Method});
                Fn -> Fn(Obj,Param)
            end;
        Fn -> Fn(Obj,Param)
    end.


