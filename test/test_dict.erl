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
    Dict2 = eos:new(eos_dictobj,[],[{a , alpha },{b , beta },{c , gamma }]),
    Dict = Dict2.
