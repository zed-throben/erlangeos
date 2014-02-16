-module(eos_etsutil).
-compile(export_all).

clone(Src,Dst)->
    ets:foldl( fun(X,Acc)-> ets:insert(Dst,X) end,[],Src ).
