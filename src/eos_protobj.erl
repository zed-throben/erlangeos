-module(eos_protobj).
-compile(export_all).
-include("eos.hrl").

create(Params)->
    Obj = new(),
    lists:foreach(
        fun({Key,Value})->
            set_slot(Obj,Key,Value) end,
        Params),
    Obj.

ets_pid(?eos(eos_protobj,PID))->
    PID.

clone(?eos(eos_protobj,PID))->
    NewObj = new(),
    eos_etsutil:clone(PID,ets_pid(NewObj)),
    NewObj.

new()->
    PID = ets:new(undefined,[set,public]),
    ?eos(eos_protobj,PID).

% プロトタイプをベースに持つオブジェクトを作成する
new({eos,eos_protobj,_}=Proto)->
    Obj = new(),
    set_slot(Obj,'_proto',Proto),
    Obj.

% 新しいプロトタイプを作成する
new_proto(TypeName)->
    Obj = new([]),
    instance_set(Obj,'_type',TypeName),
    Obj.

% 別のプロトタイプを継承（コピー）して、新しいプロトタイプを作成する
new_proto(TypeName,?eos(eos_protobj,_)=ProtoObj)->
    Obj = new(),
    eos_etsutil:clone( ets_pid(ProtoObj),ets_pid(Obj) ),
    instance_set(Obj,'_type',TypeName),
    Obj.


proto(?eos(eos_protobj,PID)=Obj)->
    case instance_get(Obj,'_proto') of
    undefined -> Obj;
    X -> X
    end.

static_set(?eos(eos_protobj,PID)=Obj,Key,Value)->
    instance_set( proto(Obj),Key,Value ).

static_get(?eos(eos_protobj,PID)=Obj,Key)->
    instance_get( proto(Obj),Key ).

static_member(?eos(eos_protobj,PID)=Obj,Key)->
    instance_member( proto(Obj),Key ).


    
instance_set(?eos(eos_protobj,PID),Key,Value)->
    ets:insert(PID,{Key,Value}).

instance_get(?eos(eos_protobj,PID),Key)->
    case ets:lookup(PID,Key) of
        [] -> undefined;
        [{Key,Value}] -> Value
    end.

instance_member(?eos(eos_protobj,PID),Key)->
    ets:member(PID,Key).


set_slot(?eos(eos_protobj,PID)=Obj,Key,Value)->
    case static_member(Obj,Key) of
        true  -> static_set(Obj,Key,Value);
        false -> instance_set(Obj,Key,Value)
    end.

get_slot(?eos(eos_protobj,PID)=Obj,Key)->
    case instance_get(Obj,Key) of
        undefined -> static_get(Obj,Key);
        Value -> Value
    end.

invoke(?eos(eos_protobj,PID)=Obj,Member,Param)->
    case get_slot(Obj,Member) of
        undefined ->
            case get_slot(Obj,'method_missing') of
                undefined -> erlang:error( {undefined_method,Member} );
                Fn -> Fn(Obj,Member,Param)
            end;
        Fn ->
            Fn(Obj,Param)
    end.

%

test1()->
    Object = eos_protobj:new(),
    eos_protobj:set_slot(Object,toString,fun(This)->"this is object." end),


    AnimalClass = eos_protobj:new_proto(animal,Object),
    eos_protobj:set_slot(AnimalClass,toString,fun(This)->"this is animal." end),

    DogClass = eos_protobj:new_proto(dog,AnimalClass),
    Dog = eos_protobj:new(DogClass).
    
test2()->
    BankAccount = eos_protobj:new(),
    eos_protobj:set_slot(BankAccount,init_user,fun(This,Arguments)->
        [Name] = Arguments,
        io:format("init_user: ~p\n",[Name]),
        eos_protobj:set_slot(This,username,Name)
    end),
    eos_protobj:set_slot(BankAccount,deposit,fun(This,Arguments)->
        [Amount] = Arguments,
        NewBalance = eos_protobj:get_slot(This,balance)+Amount,
        io:format("deposit: ~p -> ~p\n",[eos_protobj:get_slot(This,username),NewBalance]),
        eos_protobj:set_slot(This,balance,NewBalance)
    end),
    eos_protobj:set_slot(BankAccount,withdraw,fun(This,Arguments)->
        [Amount] = Arguments,
        NewBalance = eos_protobj:get_slot(This,balance)-Amount,
        io:format("withdraw: ~p -> ~p\n",[eos_protobj:get_slot(This,username),NewBalance]),
        eos_protobj:set_slot(This,balance,NewBalance)
    end),
    eos_protobj:set_slot(BankAccount,addInterest,fun(This,Arguments)->
        NewBalance = trunc( eos_protobj:get_slot(This,balance) * eos_protobj:get_slot(This,interestRate) ),
        io:format("addInterest: ~p -> ~p\n",[eos_protobj:get_slot(This,username),NewBalance]),
        eos_protobj:set_slot(This,balance,NewBalance)
    end),

io:format("##0\n",[]),

    BankAccountObj = eos_protobj:new(BankAccount),
    eos_protobj:set_slot(BankAccountObj,interestRate,1.07),
    eos_protobj:set_slot(BankAccountObj,balance,0),

io:format("##1\n",[]),

    Yasuo = eos_protobj:clone(BankAccountObj),
    eos_protobj:invoke(Yasuo,init_user,["Yasuo"]),
    eos_protobj:invoke(Yasuo,deposit,[100000000]),
    eos_protobj:invoke(Yasuo,withdraw,[1000000]),
    eos_protobj:invoke(Yasuo,addInterest,[]),

    Kaori = eos_protobj:clone(BankAccountObj),
    eos_protobj:invoke(Kaori,init_user,["Yasuo"]),
    eos_protobj:invoke(Kaori,deposit,[200000000]),
    eos_protobj:invoke(Kaori,withdraw,[2000000]),
    eos_protobj:invoke(Kaori,addInterest,[]),

    true.   

test()->
io:format("@@1\n",[]),
    test1(),
io:format("@@2\n",[]),
    test2().
