- module('test_symbol').
- compile(export_all).


test() ->
    '192.168.0.1@main' = '192.168.0.1@main',
    '192.168.0.1@main' = '192.168.0.1@main',
    './-+*=<>%$#&:;' = './-+*=<>%$#&:;',
    ['./-+*=<>%$#&:;','./-+*=<>%$#&:;'] = ['./-+*=<>%$#&:;','./-+*=<>%$#&:;'].
