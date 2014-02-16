-module(erleos_std).
-compile(export_all).

print(X)->
	io:format("~s",[X]).

println(X)->
	io:format("~s\n",[X]).
