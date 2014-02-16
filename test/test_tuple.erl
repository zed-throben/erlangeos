- module('test_tuple').
- compile(export_all).


test() ->
    {1,2,3,4} = {1,2,3,4},
    {1,2,3,4} = {1,2,3,4},
    {1,2,3,4} = {1,2,3,4}.
