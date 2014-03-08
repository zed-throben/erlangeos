- module('test_consult').
- compile(export_all).

test() ->
    A = [{a , [1,2,3] },{b , [{aa , 100 },{bb , 200 },{cc , {4,5,6} },{dd , "str" },{ee , 'alpha-beta-gamma' }] }],
    BB = <<"    #{\n        a=[1,2,3]\n        b=#{\n            aa=100\n            bb=200\n            cc={4,5,6}\n            dd=\"str\"\n            ee=:alpha-beta-gamma\n        }\n    }\n    ">>,
    B = erleos:to_term(BB),
    A = B,
    [1,2,3] = eos:get_slot(A,a),
    {4,5,6} = eos:get_slot(B,cc),
    100 = eos:get_slot(eos:get_slot(A,b),aa),
    200 = eos:get_slot(eos:get_slot(A,b),bb),
    "str" = eos:get_slot(eos:get_slot(A,b),dd),
    'alpha-beta-gamma' = eos:get_slot(eos:get_slot(A,b),ee).
