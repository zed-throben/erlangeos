- module('io_inheritance').
- compile(export_all).


start() ->
    Dog = eos:new(eos_obj,[],[{barkPhrase , "woof!" },{bark , fun (This,Arguments) ->
        io:format(eosstd:fmt("~s\n",[eosstd:to_str(eos:get_slot(This,barkPhrase))])) end }]),
    Chiwawa = eos:invoke(Dog,clone,[]),
    eos:set_slot(Chiwawa,barkPhrase , "yip!"),
    io:format("Dog bark: "),
    eos:invoke(Dog,bark,[]),
    io:format("Chiwawa bark: "),
    eos:invoke(Chiwawa,bark,[]),
    MyChiwawa = eos:invoke(Chiwawa,clone,[]),
    eos:set_slot(MyChiwawa,barkPhrase , "Yo Quiero Taco Bell"),
    io:format("myChiwawa bark: "),
    eos:invoke(MyChiwawa,bark,[]).
