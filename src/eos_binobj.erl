-module(eos_binobj).
-compile(export_all).

length(Bin)->
	erlang:byte_size(Bin).

to_bin(Bin)->
	Bin.

to_list(Bin)->
	erlang:binary_to_list(Bin).

to_atom(Bin,Encoding)->
	erlang:binary_to_atom(Bin,Encoding).

to_atom(Bin)->
	to_atom(Bin,utf8).

to_term(Bin)->
	erlang:binary_to_term(Bin).
