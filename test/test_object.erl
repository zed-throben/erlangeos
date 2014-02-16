- module('test_object').
- compile(export_all).


test() ->
    A = eos:create(eos_protobj ,[{hello , fun (This,Arguments) ->[X|RestArguments] = Arguments,
        io:format("hello ~s\n",[X]) end },{set , fun (This,Arguments) ->[X|RestArguments] = Arguments,
        eos:set_slot(This,a , X),
        eos:set_slot(This,b , X * 2 ) end },{add , fun (This,Arguments) ->
        eos:get_slot(This,a) + eos:get_slot(This,b)  end }]),
    io:format("~p\n",[eos:get_slot(A,hello)]),
    eos:invoke(A,hello,["world"]),
    eos:invoke(A,set,[10]),
    30 = eos:invoke(A,add,[]).
