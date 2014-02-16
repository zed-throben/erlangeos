-module(erleos_module_parser).
-compile(export_all).
-include("eos_modobj.hrl").
-include("erleos_parser.hrl").

init_module_object()->
    [].

simplify( ?t(symbol,X) )->X.

parse_toplevel([])->
    true;

parse_toplevel([ ?t(definition,{ ?t(symbol,module),[Param]})|T] )->
	put(module,simplify(Param) ),
	parse_toplevel(T);

parse_toplevel([ ?t(definition,{ ?t(symbol,export),Param} )|T] )->
	put(export,Param ),
	parse_toplevel(T);

parse_toplevel( [H|T] )->
	%io:format("**skip ~p\n",[H]),
    parse_toplevel(T).

%
