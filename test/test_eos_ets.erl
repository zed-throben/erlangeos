- module('test_eos_ets').
- compile(export_all).


foo(N) ->
    Obj = eos:new(eos_ets,[],[{accumulator , N }]),
    fun (I) -> 
        eos:set_slot(Obj,accumulator , eos:get_slot(Obj,accumulator) + I ) end.


foo2(N) ->
    Obj = eos:new(eos_obj,[],[{accumulator , N },{f , fun (This,Arguments) ->[I|RestArguments] = Arguments,
        eos:set_slot(This,accumulator , eos:get_slot(This,accumulator) + I ) end }]),
    fun (I) -> 
        eos:invoke(Obj,f,[I]) end.


test() ->
    A = eos:new(eos_ets,[],[{uho , fun (This,Arguments) ->
        io:format("iiotoko\n",[]) end },{hello , fun (This,Arguments) ->[X|RestArguments] = Arguments,
        io:format("hello ~s\n",[X]) end },{set , fun (This,Arguments) ->[X|RestArguments] = Arguments,
        eos:set_slot(This,a , X),
        eos:set_slot(This,b , X * 2 ) end },{add , fun (This,Arguments) ->
        eos:get_slot(This,a) + eos:get_slot(This,b)  end }]),
    io:format("~p\n",[eos:get_slot(A,hello)]),
    eos:invoke(A,hello,["world"]),
    eos:invoke(A,uho,[]),
    eos:invoke(A,set,[10]),
    30 = eos:invoke(A,add,[]),
    Foo = foo(0),
    1 = Foo(1),
    2 = Foo(1),
    12 = Foo(10),
    22 = Foo(10),
    Foo2 = foo2(0),
    1 = Foo2(1),
    2 = Foo2(1),
    12 = Foo2(10),
    22 = Foo2(10).
