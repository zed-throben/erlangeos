- module('accgen2').
- compile(export_all).


foo(N) ->
    Obj = eos:new(eos@obj,[],[{accumulator , N }]),
    fun (I) -> 
        eos:set_slot(Obj,accumulator , eos:get_slot(Obj,accumulator) + I ) end.


test() ->
    Foo = foo(0),
    1 = Foo(1),
    2 = Foo(1),
    12 = Foo(10),
    22 = Foo(10).
