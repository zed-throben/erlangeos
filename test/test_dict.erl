- module('test_dict').
- compile(export_all).


test() ->
    PropList = [{a,alpha},{b,beta},{c,gamma}],
    3 = eos:invoke(PropList,length,[]),
    alpha = eos:get_slot(PropList,a),
    beta = eos:get_slot(PropList,b),
    gamma = eos:get_slot(PropList,c),
    Dict = dict:from_list(PropList),
    3 = eos:invoke(Dict,length,[]),
    alpha = eos:get_slot(Dict,a),
    beta = eos:get_slot(Dict,b),
    gamma = eos:get_slot(Dict,c),
    PropList2 = [{a , alpha },{b , beta },{c , gamma }],
    PropList = PropList2,
    Dict2 = eos:new(eos@dict,[],[{a , alpha },{b , beta },{c , gamma }]),
    Dict = Dict2,
    Dict3 = eos:set_slot(Dict,a , aaa),
    Dict4 = eos:set_slot(Dict3,d , ddd),
    aaa = eos:get_slot(Dict4,a),
    ddd = eos:get_slot(Dict4,d),
    Dict5 = eos:invoke(Dict4,map,[fun (Key,Value) -> 
        eosstd:fmt("~s -> ~s",[eosstd:to_str(Key),eosstd:to_str(Value)]) end]),
    "a -> aaa" = eos:get_slot(Dict5,a),
    "b -> beta" = eos:get_slot(Dict5,b),
    "c -> gamma" = eos:get_slot(Dict5,c),
    "d -> ddd" = eos:get_slot(Dict5,d).
