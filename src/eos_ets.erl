-module(eos_ets).
-compile(export_all).
-include("eos.hrl").

new(Options,Params)->
    Obj = new(),
    lists:foreach(
        fun({Key,Value})->
            set_slot(Obj,Key,Value) end,
        Params),
    Obj.

ets_pid(?eos(eos_ets,PID))->
    PID.

new()->
    PID = ets:new(undefined,[set,public]),
    ?eos(eos_ets,PID).

clone(?eos(eos_ets,PID)=Obj)->
    NewObj = new(),
    eos_etsutil:clone(PID,ets_pid(NewObj)),
    NewObj.
    
set_slot(?eos(eos_ets,PID),Key,Value)->
    ets:insert(PID,{Key,Value}),
    Value.

get_slot(?eos(eos_ets,PID),Key)->
    case ets:lookup(PID,Key) of
        [{Key,Value}] -> Value;
        _ -> undefined
    end.

modify(?eos(eos_ets,PID)=Obj,Key,Fn)->
    NewVal = Fn( get_slot(Obj,Key) ),
    set_slot(Obj,Key,NewVal),
    NewVal.

invoke(?eos(eos_ets,PID)=Obj,Member,Param)->
    case get_slot(Obj,Member) of
        undefined ->
            case get_slot(Obj,missing_method) of
                undefined -> erlang:error({missing_method,Member});
                Fn -> Fn(Obj,Param)
            end;
        Fn -> Fn(Obj,Param)
    end.

test1()->
    Obj = eos_ets:new(),
    eos_ets:set_slot(Obj,users,eos_ets:new()),
    eos_ets:set_slot(Obj,add_user,fun(This,Arguments)->
        [Name,Password] = Arguments,
        io:format("add user ~p : ~p\n",[Name,Password]),
        eos_ets:set_slot( eos_ets:get_slot(This,users),Name,Password )
    end),

    eos_ets:set_slot(Obj,login,fun(This,Arguments)->
        [Name,Password] = Arguments,
        CorrectPassword = eos_ets:get_slot( eos_ets:get_slot(This,users),Name ),
        if  CorrectPassword == Password ->
                io:format("login ok: ~p\n",[Name]),
                true;
            true ->
                io:format("login ng: ~p\n",[Name]),
                false
        end
    end
    ),
    
    eos_ets:call(Obj,add_user,[yasuo,uho]),
    eos_ets:call(Obj,add_user,[kaori,uhi]),
    eos_ets:call(Obj,login,[yasuo,uho]),
    eos_ets:call(Obj,login,[kaori,uho]).

test2()->
    BankAccount = eos_ets:new(),
    eos_ets:set_slot(BankAccount,interestRate,1.07),
    eos_ets:set_slot(BankAccount,balance,0),

    eos_ets:set_slot(BankAccount,init_user,fun(This,Arguments)->
        [Name] = Arguments,
        io:format("init_user: ~p\n",[Name]),
        eos_ets:set_slot(This,username,Name)
    end),
    eos_ets:set_slot(BankAccount,deposit,fun(This,Arguments)->
        [Amount] = Arguments,
        NewBalance = eos_ets:get_slot(This,balance)+Amount,
        io:format("deposit: ~p -> ~p\n",[eos_ets:get_slot(This,username),NewBalance]),
        eos_ets:set_slot(This,balance,NewBalance)
    end),
    eos_ets:set_slot(BankAccount,withdraw,fun(This,Arguments)->
        [Amount] = Arguments,
        NewBalance = eos_ets:get_slot(This,balance)-Amount,
        io:format("withdraw: ~p -> ~p\n",[eos_ets:get_slot(This,username),NewBalance]),
        eos_ets:set_slot(This,balance,NewBalance)
    end),
    eos_ets:set_slot(BankAccount,addInterest,fun(This,Arguments)->
        NewBalance = trunc( eos_ets:get_slot(This,balance) * eos_ets:get_slot(This,interestRate) ),
        io:format("addInterest: ~p -> ~p\n",[eos_ets:get_slot(This,username),NewBalance]),
        eos_ets:set_slot(This,balance,NewBalance)
    end),

    Yasuo = eos_ets:clone(BankAccount),
    eos_ets:call(Yasuo,init_user,["Yasuo"]),
    eos_ets:call(Yasuo,deposit,[100000000]),
    eos_ets:call(Yasuo,withdraw,[1000000]),
    eos_ets:call(Yasuo,addInterest,[]),

    Kaori = eos_ets:clone(BankAccount),
    eos_ets:call(Kaori,init_user,["Yasuo"]),
    eos_ets:call(Kaori,deposit,[200000000]),
    eos_ets:call(Kaori,withdraw,[2000000]),
    eos_ets:call(Kaori,addInterest,[]),

    true.

test3()->
    AccumulatorGenerator = fun(InitialValue)->
        Obj = eos_ets:new(),
        eos_ets:set_slot(Obj,value,InitialValue),
        fun(N)->
            eos_ets:modify(Obj,value,fun(Val)->Val+N end)
        end
    end,

    A = AccumulatorGenerator(100),
    B = AccumulatorGenerator(200),

    io:format("A: ~p\n",[ A(1) ] ),
    io:format("A: ~p\n",[ A(2) ] ),
    io:format("A: ~p\n",[ A(3) ] ),
            
    io:format("B: ~p\n",[ B(1) ] ),
    io:format("B: ~p\n",[ B(2) ] ),
    io:format("B: ~p\n",[ B(3) ] ),

    true.


test()->
    test1(),
    test2(),
    test3(),
    true.


        
