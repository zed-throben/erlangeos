-module(erleos_formatted_string).
-compile(export_all).

% \{Var}
% \{~p|Var}

split([],_,Acc)->
	{ lists:reverse(Acc),[] };

split([A|T],A,Acc)->
	{ lists:reverse(Acc),T };

split([H|T],A,Acc)->
	split(T,A,[H|Acc]).

split(Str,A)->
	split(Str,A,[]).

%

parse_format([$~|_]=Str)->
	{Fmt,Expr} = split(Str,$|);

parse_format(Str)->
	{[],Str}.

%

extract_format([$}|T],Acc)->
	{ parse_format(lists:reverse(Acc)),T};

extract_format([H|T],Acc)->
	extract_format(T,[H|Acc]).


%

extract_string([],Acc)->
	{lists:reverse(Acc),[]};

extract_string([$#,${ |T]=Src,Acc)->
	{lists:reverse(Acc),Src};

extract_string([H|T],Acc)->
	extract_string(T,[H|Acc]).

%

parse([],Acc)->
	lists:reverse(Acc);

parse([$#,${ |T],Acc)->
	{A,Rest} = extract_format(T,[]),
	parse( Rest,[ A|Acc] );

parse(Str,Acc)->
	{A,Rest} = extract_string(Str,[]),
	parse( Rest,[ A|Acc ] ).

parse(Str)->
	parse(Str,[]).

%

to_format([],Acc)->
	Acc;

to_format([{[],Expr}|T],Acc)->
	to_format(T,Acc ++ "~s");

to_format([{Fmt,Expr}|T],Acc)->
	to_format(T,Acc ++ Fmt);

to_format([H|T],Acc)->
	to_format(T,Acc++H).

to_format(Str)->
	L = parse(Str),
	to_format(L,[]).

%
to_param([],Acc)->
	lists:reverse(Acc);

to_param([{[],Expr}|T],Acc)->
	to_param(T,[ "eosstd.to_str(" ++ Expr ++ ")" | Acc]);

to_param([{Fmt,Expr}|T],Acc)->
	to_param(T,[Expr|Acc]);

to_param([H|T],Acc)->
	to_param(T,Acc).

to_param(Str)->
	L = parse(Str),
	to_param(L,[]).
	
%

convert(Str)->
	L = parse( eosstd:to_list(Str) ),
	{ to_format(L,[]) , to_param(L,[]) }.


test()->
	formatted_string:convert("hello #{Name} #{~p|Uho}!!").
	