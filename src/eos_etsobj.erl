-module(eos_etsobj).
-compile(export_all).
-include("eos.hrl").

create(Params)->
    Obj = new(),
    lists:foreach(
        fun({Key,Value})->
            set_slot(Obj,Key,Value) end,
        Params),
    Obj.

ets_pid(?eos(eos_etsobj,PID))->
    PID.

new()->
    PID = ets:new(undefined,[set,public]),
    ?eos(eos_etsobj,PID).

new(FnInit) when is_function(FnInit) ->
    ETS = new(),
    FnInit(ETS),
    ETS.

clone(?eos(eos_etsobj,PID)=Obj)->
    NewObj = new(),
    eos_etsutil:clone(PID,ets_pid(NewObj)),
    NewObj.
    
set_slot(?eos(eos_etsobj,PID),Key,Value)->
    ets:insert(PID,{Key,Value}).

get_slot(?eos(eos_etsobj,PID),Key)->
    case ets:lookup(PID,Key) of
        [{Key,Value}] -> Value;
        _ -> undefined
    end.

modify(?eos(eos_etsobj,PID)=Obj,Key,Fn)->
    NewVal = Fn( get_slot(Obj,Key) ),
    set_slot(Obj,Key,NewVal),
    NewVal.

invoke(?eos(eos_etsobj,PID)=Obj,Member,Param)->
    case get_slot(Obj,Member) of
    undefined ->
        case get_slot(Obj,missing_method) of
            undefined -> erlang:error(missing_method);
            Fn -> Fn(Obj,Param)
        end;
    Fn -> Fn(Obj,Param)
    end.

test1()->
    Obj = eos_etsobj:new(),
    eos_etsobj:set_slot(Obj,users,eos_etsobj:new()),
    eos_etsobj:set_slot(Obj,add_user,fun(This,Arguments)->
        [Name,Password] = Arguments,
        io:format("add user ~p : ~p\n",[Name,Password]),
        eos_etsobj:set_slot( eos_etsobj:get_slot(This,users),Name,Password )
    end),

    eos_etsobj:set_slot(Obj,login,fun(This,Arguments)->
        [Name,Password] = Arguments,
        CorrectPassword = eos_etsobj:get_slot( eos_etsobj:get_slot(This,users),Name ),
        if  CorrectPassword == Password ->
                io:format("login ok: ~p\n",[Name]),
                true;
            true ->
                io:format("login ng: ~p\n",[Name]),
                false
        end
    end
    ),
    
    eos_etsobj:call(Obj,add_user,[yasuo,uho]),
    eos_etsobj:call(Obj,add_user,[kaori,uhi]),
    eos_etsobj:call(Obj,login,[yasuo,uho]),
    eos_etsobj:call(Obj,login,[kaori,uho]).

test2()->
    BankAccount = eos_etsobj:new(),
    eos_etsobj:set_slot(BankAccount,interestRate,1.07),
    eos_etsobj:set_slot(BankAccount,balance,0),

    eos_etsobj:set_slot(BankAccount,init_user,fun(This,Arguments)->
        [Name] = Arguments,
        io:format("init_user: ~p\n",[Name]),
        eos_etsobj:set_slot(This,username,Name)
    end),
    eos_etsobj:set_slot(BankAccount,deposit,fun(This,Arguments)->
        [Amount] = Arguments,
        NewBalance = eos_etsobj:get_slot(This,balance)+Amount,
        io:format("deposit: ~p -> ~p\n",[eos_etsobj:get_slot(This,username),NewBalance]),
        eos_etsobj:set_slot(This,balance,NewBalance)
    end),
    eos_etsobj:set_slot(BankAccount,withdraw,fun(This,Arguments)->
        [Amount] = Arguments,
        NewBalance = eos_etsobj:get_slot(This,balance)-Amount,
        io:format("withdraw: ~p -> ~p\n",[eos_etsobj:get_slot(This,username),NewBalance]),
        eos_etsobj:set_slot(This,balance,NewBalance)
    end),
    eos_etsobj:set_slot(BankAccount,addInterest,fun(This,Arguments)->
        NewBalance = trunc( eos_etsobj:get_slot(This,balance) * eos_etsobj:get_slot(This,interestRate) ),
        io:format("addInterest: ~p -> ~p\n",[eos_etsobj:get_slot(This,username),NewBalance]),
        eos_etsobj:set_slot(This,balance,NewBalance)
    end),

    Yasuo = eos_etsobj:clone(BankAccount),
    eos_etsobj:call(Yasuo,init_user,["Yasuo"]),
    eos_etsobj:call(Yasuo,deposit,[100000000]),
    eos_etsobj:call(Yasuo,withdraw,[1000000]),
    eos_etsobj:call(Yasuo,addInterest,[]),

    Kaori = eos_etsobj:clone(BankAccount),
    eos_etsobj:call(Kaori,init_user,["Yasuo"]),
    eos_etsobj:call(Kaori,deposit,[200000000]),
    eos_etsobj:call(Kaori,withdraw,[2000000]),
    eos_etsobj:call(Kaori,addInterest,[]),

    true.

test3()->
    AccumulatorGenerator = fun(InitialValue)->
        Obj = eos_etsobj:new(),
        eos_etsobj:set_slot(Obj,value,InitialValue),
        fun(N)->
            eos_etsobj:modify(Obj,value,fun(Val)->Val+N end)
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


        
