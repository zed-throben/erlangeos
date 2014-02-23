- module('io_account').
- compile(export_all).


start() ->
    Account = eos:new(eos@obj,[],[{balance , 0.0 },{deposit , fun (This,Arguments) ->[V|RestArguments] = Arguments,
        eos:set_slot(This,balance , eos:get_slot(This,balance) + V ) end },{show , fun (This,Arguments) ->
        eosstd:fmt("Account balance: $~s\n",[eosstd:to_str(eos:get_slot(This,balance))]) end }]),
    io:format(eosstd:fmt("Inital: ~s",[eosstd:to_str(eos:invoke(Account,show,[]))])),
    io:format("Depositing $10\n"),
    eos:invoke(Account,deposit,[10.0]),
    io:format(eosstd:fmt("Final: ~s",[eosstd:to_str(eos:invoke(Account,show,[]))])).
