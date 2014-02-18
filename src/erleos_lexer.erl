-module(erleos_lexer).
-compile(export_all).
-include("erleos_parser.hrl").
%
%
%


f(F,P)->
	eosstd:lsformat(F,P).

to_charlist(X)->
	unicode:characters_to_list(X).


p(X)->
	io:format("~p~n",[X]).

err(X)->
	io:format("err: ~p~n",[X]).

%
%

%escch(Ch)->
%	case Ch of
%		$\\ -> "\\\\";
%		$" -> "\"";
%		$' -> "\'";
%		_ -> Ch
%	end.
%
%escape(Str)->
%	lists:flatten( lists:map(fun escch/1,Str) ).

esc([],Acc)->
	lists:flatten( lists:reverse(Acc) );
esc([$"|T],Acc)->
	esc(T,["\\\""|Acc]);
esc([$'|T],Acc)->
	esc(T,["\\\'"|Acc]);
esc([$\\|T],Acc)->
	esc(T,["\\\\"|Acc]);
esc([10,13|T],Acc)->
	esc(T,["\\n"|Acc]);
esc([13,10|T],Acc)->
	esc(T,["\\n"|Acc]);
esc([13|T],Acc)->
	esc(T,["\\n"|Acc]);
esc([10|T],Acc)->
	esc(T,["\\n"|Acc]);
esc([H|T],Acc)->
	esc(T,[H|Acc]).

escape(Str)->
	esc(Str,[]).



%
%


nextline([10,13|T]	,Acc,{Line,Row} = LnRw)->
	lex(T,Acc,{Line+1,0} );	
nextline([13,10|T]	,Acc,{Line,Row} = LnRw)->
	lex(T,Acc,{Line+1,0} );	
nextline([10|T]	,Acc,{Line,Row} = LnRw)->
	lex(T,Acc,{Line+1,0} );	
nextline([13|T]	,Acc,{Line,Row} = LnRw)->
	lex(T,Acc,{Line+1,0} );	
nextline([_|T]	,Acc,{Line,Row} = LnRw)->
	nextline(T,Acc,LnRw ).


skipcomment([$*,$/|T]	,Acc,{Line,Row} = LnRw)->
	lex(T,Acc,{Line,Row+2} );	
skipcomment([10,13|T]	,Acc,{Line,Row} = LnRw)->
	skipcomment(T,Acc,{Line+1,0} );	
skipcomment([13,10|T]	,Acc,{Line,Row} = LnRw)->
	skipcomment(T,Acc,{Line+1,0} );	
skipcomment([10|T]	,Acc,{Line,Row} = LnRw)->
	skipcomment(T,Acc,{Line+1,0} );	
skipcomment([13|T]	,Acc,{Line,Row} = LnRw)->
	skipcomment(T,Acc,{Line+1,0} );	
skipcomment([_|T]	,Acc,{Line,Row} = LnRw)->
	skipcomment(T,Acc,{Line,Row+1} ).

%
%
%



% decode "STRING" 
%
return_until_eol(T,Acc,{Line,Row})->
	{T,lists:reverse(Acc),{Line+1,0} }.
until_eol([10,13|T],Acc,{Line,Row} = LnRw)->
	return_until_eol(T,Acc,LnRw);
until_eol([13,10|T],Acc,{Line,Row} = LnRw)->
	return_until_eol(T,Acc,LnRw);
until_eol([10|T],Acc,{Line,Row} = LnRw)->
	return_until_eol(T,Acc,LnRw);
until_eol([13|T],Acc,{Line,Row} = LnRw)->
	return_until_eol(T,Acc,LnRw);
until_eol([H|T],Acc,{Line,Row} = LnRw)->
	until_eol(T,[H|Acc],{Line,Row+1} ).

%
%

start_with(_,[])->
	true;
start_with([],_)->
	false;
start_with([H|T1],[H|T2])->
	start_with(T1,T2);
start_with(_,_)->
	false.

until_symbol([],Acc,{Line,Row} = LnRw,_)->
	{ [], lists:reverse(Acc),{Line,Row} };

until_symbol([10,13|T],Acc,{Line,Row} = LnRw,SYMBOL)->
	until_symbol(T,[10,13|Acc],{Line+1,0},SYMBOL);
until_symbol([13,10|T],Acc,{Line,Row} = LnRw,SYMBOL)->
	until_symbol(T,[13,10|Acc],{Line+1,0},SYMBOL);
until_symbol([10|T],Acc,{Line,Row} = LnRw,SYMBOL)->
	until_symbol(T,[10|Acc],{Line+1,0},SYMBOL);
until_symbol([13|T],Acc,{Line,Row} = LnRw,SYMBOL)->
	until_symbol(T,[13|Acc],{Line+1,0},SYMBOL);
until_symbol([H|T]=Src,Acc,{Line,Row} = LnRw,SYMBOL)->
	case start_with(Src,SYMBOL) of
		true ->
			SymLen = length(SYMBOL),
			{ lists:nthtail( SymLen,Src), lists:reverse(Acc),{Line,Row+SymLen} };
		_ ->
			until_symbol(T,[H|Acc],{Line,Row+1},SYMBOL )
	end.


%
% decode "direct" 
%
lex_direct([$%,$%|T],Acc,{Line,Row} = LnRw)->
	{T,lists:reverse( Acc ),{Line,Row} };
lex_direct([10,13|T],Acc,{Line,Row} = LnRw)->
	lex_direct(T,[10,13|Acc],{Line+1,0});
lex_direct([13,10|T],Acc,{Line,Row} = LnRw)->
	lex_direct(T,[13,10|Acc],{Line+1,0});
lex_direct([10|T],Acc,{Line,Row} = LnRw)->
	lex_direct(T,[10|Acc],{Line+1,0});
lex_direct([13|T],Acc,{Line,Row} = LnRw)->
	lex_direct(T,[13|Acc],{Line+1,0});
lex_direct([H|T],Acc,{Line,Row} = LnRw)->
	lex_direct(T,[H|Acc],{Line,Row+1} ).


%
% decode "STRING" 
%
lex_string([$\\,Ch|T],Acc,{Line,Row} = LnRw)->
	lex_string(T,[Ch,$\\|Acc],{Line,Row+2});
lex_string([$"|T],Acc,{Line,Row} = LnRw)->
	{T,lists:reverse( Acc ),{Line,Row} };
lex_string([10,13|T],Acc,{Line,Row} = LnRw)->
	lex_string(T,[10,13|Acc],{Line+1,0});
lex_string([13,10|T],Acc,{Line,Row} = LnRw)->
	lex_string(T,[13,10|Acc],{Line+1,0});
lex_string([10|T],Acc,{Line,Row} = LnRw)->
	lex_string(T,[10|Acc],{Line+1,0});
lex_string([13|T],Acc,{Line,Row} = LnRw)->
	lex_string(T,[13|Acc],{Line+1,0});
lex_string([H|T],Acc,{Line,Row} = LnRw)->
	lex_string(T,[H|Acc],{Line,Row+1} ).



%
% decode 'STRING' 
%
lex_string2([$\\,Ch|T],Acc,{Line,Row} = LnRw)->
	lex_string2(T,[Ch,$\\|Acc],{Line,Row+2});
lex_string2([$'|T],Acc,{Line,Row} = LnRw)->
	{T,lists:reverse( Acc ),{Line,Row} };
lex_string2([10,13|T],Acc,{Line,Row} = LnRw)->
	lex_string2(T,[10,13|Acc],{Line+1,0});
lex_string2([13,10|T],Acc,{Line,Row} = LnRw)->
	lex_string2(T,[13,10|Acc],{Line+1,0});
lex_string2([10|T],Acc,{Line,Row} = LnRw)->
	lex_string2(T,[10|Acc],{Line+1,0});
lex_string2([13|T],Acc,{Line,Row} = LnRw)->
	lex_string2(T,[13|Acc],{Line+1,0});
lex_string2([H|T],Acc,{Line,Row} = LnRw)->
	lex_string2(T,[H|Acc],{Line,Row+1} ).

%
%
%

is_l_alpha(C)->
	($a =< C) and (C =< $z).

is_u_alpha(C)->
	($A =< C) and (C =< $Z).

is_alpha(C)->
	is_l_alpha(C) or is_u_alpha(C).

is_digit(C)->
	($0 =< C) and (C =< $9).

is_whitespace(C)->
	(C == 9) or (C == 10) or (C == 13) or (C ==32).

is_initial(C)->
	%is_alpha(C) or (C == $_).
	is_alpha(C) or (C == $_) or (C == $?).	% $? preprocessor

is_name(C)->
	is_initial(C) or is_digit(C) or (C == $@).

%

lex_symbol([],Acc,{Line,Row} = LnRw)->
	{ [],list_to_atom( lists:reverse(Acc) ),LnRw };

lex_symbol([H|T] = Rest,[]=Acc,{Line,Row} = LnRw)->
	Continuable = is_initial(H),
	if	Continuable -> lex_symbol(T,[H|Acc],{Line,Row+1} );
		true -> { Rest,list_to_atom( lists:reverse(Acc) ),LnRw }
	end;

lex_symbol([H|T] = Rest,Acc,{Line,Row} = LnRw)->
	Continuable = is_name(H) or (H == $/),
	if	Continuable -> lex_symbol(T,[H|Acc],{Line,Row+1} );
		true -> { Rest,list_to_atom( lists:reverse(Acc) ),LnRw }
	end.
 	
lex_symbol(Src,{Line,Row} = LnRw)->
	lex_symbol(Src,[],LnRw).


% symbol_w lasts until ()[],

lex_symbol_w([],Acc,{Line,Row} = LnRw)->
	{ [],list_to_atom( lists:reverse(Acc) ),LnRw };

lex_symbol_w([H|T] = Rest,Acc,{Line,Row} = LnRw)->
	Continuable =	is_name(H) or
					case H of
						$. -> true;
						$/ -> true;
						$- -> true;
						$+ -> true;
						$* -> true;
						$= -> true;
						$< -> true;
						$> -> true;
						$% -> true;
						$$ -> true;
						$# -> true;
						$& -> true;
						$: -> true;
						$; -> true;
						_  -> false
					end,
	if	Continuable -> lex_symbol_w(T,[H|Acc],{Line,Row+1} );
		true -> { Rest,list_to_atom( lists:reverse(Acc) ),LnRw }
	end.
 	
lex_symbol_w(Src,{Line,Row} = LnRw)->
	lex_symbol_w(Src,[],LnRw).

%
lex_variable([],Acc,{Line,Row} = LnRw)->
	{ [],list_to_atom( lists:reverse(Acc) ),LnRw };

lex_variable([H|T] = Rest,[]=Acc,{Line,Row} = LnRw)->
	Continuable = is_initial(H),
	if	Continuable -> lex_variable(T,[H|Acc],{Line,Row+1} );
		true -> { Rest,list_to_atom( lists:reverse(Acc) ),LnRw }
	end;

lex_variable([H|T] = Rest,Acc,{Line,Row} = LnRw)->
	Continuable = is_name(H),
	if	Continuable -> lex_variable(T,[H|Acc],{Line,Row+1} );
		true -> { Rest,list_to_atom( lists:reverse(Acc) ),LnRw }
	end.
 	
lex_variable(Src,{Line,Row} = LnRw)->
	lex_variable(Src,[],LnRw).

%
% 
%

src_to_float(LnRw,List)->
	try
		erlang:list_to_float(List)
	catch
		EType:EMsg ->
			erleos:compile_error(lexer,LnRw,"invalid float format: " ++ List )
	end.


lex_float([],Acc,{Line,Row} = LnRw )->
	{ [],{'float',src_to_float( LnRw,lists:reverse(Acc) )},LnRw  };

lex_float([H|T]=Src,Acc,{Line,Row} = LnRw)->
	IsDigit = is_digit(H),
	if	IsDigit -> lex_float(T,[H|Acc],{Line,Row+1} );
		H == $. -> throw("invalid float format");
		H == $e ->	{RetRest,RetX,RetLnRw} = lex_int_token(T,[],LnRw ),
					{RetRest,{'e_int',src_to_float(LnRw,lists:reverse(Acc)),RetX},RetLnRw};
		true -> { Src,{'float',src_to_float( LnRw,lists:reverse(Acc) )},LnRw }
	end.

lex_int([],Acc,{Line,Row} = LnRw)->
	{ [],{'int',list_to_integer( lists:reverse(Acc) )},LnRw };

lex_int([$.,$.|T]=Src,Acc,{Line,Row} = LnRw )->
	{ Src,{'int',list_to_integer( lists:reverse(Acc) )},LnRw };

lex_int([H|T]=Src,Acc,{Line,Row} = LnRw )->
	IsDigit = is_digit(H),
	if	IsDigit -> lex_int  (T,[H|Acc],{Line,Row+1} );
		H == $. -> lex_float(T,[H|Acc],{Line,Row+1} );
		H == $# ->	{RetRest,RetX,RetLnRw} = lex_token(T,[],LnRw ),
					{RetRest,{'n_int',list_to_integer(lists:reverse(Acc)),RetX},RetLnRw};
		true -> { Src,{'int',list_to_integer( lists:reverse(Acc) )},LnRw }
	end.

lex_token([],Acc,{Line,Row} = LnRw)->
	{ [],list_to_binary( lists:reverse(Acc) ),LnRw };

lex_token([H|T]=Src,Acc,{Line,Row} = LnRw )->
	case is_name(H) of
		true -> lex_token  (T,[H|Acc],{Line,Row+1} );
		_ -> { Src,list_to_binary( lists:reverse(Acc) ),LnRw }
	end.

lex_int_token([],Acc,{Line,Row} = LnRw)->
	{ [],list_to_integer( lists:reverse(Acc) ),LnRw };

lex_int_token([H|T]=Src,Acc,{Line,Row} = LnRw )->
	case is_digit(H) of
		true -> lex_int_token  (T,[H|Acc],{Line,Row+1} );
		_ -> { Src,list_to_integer( lists:reverse(Acc) ),LnRw }
	end.

lex_number(X,{Line,Row} = LnRw )->
	lex_int(X,[],LnRw ).


%
lex([],Acc,{Line,Row} = LnRw)->
	lists:reverse( Acc );

%
%lex([$<,$<,${,$},$>,$> |T],Acc,{Line,Row} = LnRw)->
%	lex(T,[?token(LnRw,'<<{}>>')|Acc],{Line,Row+6} );
%
%lex([$<,$<,${ |T],Acc,{Line,Row} = LnRw)->
%	lex(T,[?token(LnRw,'<<{')|Acc],{Line,Row+3} );
%
%lex([$},$>,$> |T],Acc,{Line,Row} = LnRw)->
%	lex(T,[?token(LnRw,'}>>')|Acc],{Line,Row+3} );

%
lex([$#,${ |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'#{')|Acc],{Line,Row+2} );

lex([$#,$< |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'#<')|Acc],{Line,Row+2} );

% delimiter
% continuous delimiter
%lex([$, |T]		,[','|Acc],{Line,Row} = LnRw)->
%	throw("err: dobule comma."),
%	lex(T,Acc,{Line,Row+1});
%lex([10,13|T]	,[{LnRw,'cr'}|Acc],{Line,Row} = LnRw)->
%	lex(T,Acc,{Line+1,0});
%lex([13,10|T]	,[{LnRw,'cr'}|Acc],{Line,Row} = LnRw)->
%	lex(T,Acc,{Line+1,0});
%lex([10|T]		,[{LnRw,'cr'}|Acc],{Line,Row} = LnRw)->
%	lex(T,Acc,{Line+1,0});
%lex([13|T]		,[{LnRw,'cr'}|Acc],{Line,Row} = LnRw)->
%	lex(T,Acc,{Line+1,0});

%
lex([$.,$.,$. |T]		,Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'...')|Acc],{Line,Row+2} );

lex([$.,$. |T]		,Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'..')|Acc],{Line,Row+2} );

lex([$. |T]		,Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'.')|Acc],{Line,Row+1} );
lex([$; |T]		,Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,';')|Acc],{Line,Row+1} );
lex([$, |T]		,Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,',')|Acc],{Line,Row+1} );
%lex([10,13|T]	,Acc,{Line,Row})->
%	lex(T,[{{Line+1,0},'cr'}|Acc],{Line+1,0});
%lex([13,10|T]	,Acc,{Line,Row})->
%	lex(T,[{{Line+1,0},'cr'}|Acc],{Line+1,0});
%lex([10|T]		,Acc,{Line,Row})->
%	lex(T,[{{Line+1,0},'cr'}|Acc],{Line+1,0});
%lex([13|T]		,Acc,{Line,Row})->
%	lex(T,[{{Line+1,0},'cr'}|Acc],{Line+1,0});
lex([10,13|T]	,Acc,{Line,Row})->
	lex(T,Acc,{Line+1,0});
lex([13,10|T]	,Acc,{Line,Row})->
	lex(T,Acc,{Line+1,0});
lex([10|T]		,Acc,{Line,Row})->
	lex(T,Acc,{Line+1,0});
lex([13|T]		,Acc,{Line,Row})->
	lex(T,Acc,{Line+1,0});

% whitespace,linefeed
lex([32|T],Acc,{Line,Row} = LnRw)->
	lex(T,Acc,{Line,Row+1});
lex([9|T],Acc,{Line,Row} = LnRw)->
	erleos:compile_error(lexer,LnRw,"tab char shouldn't be used."),
	lex(T,Acc,{Line,Row+1});

% %  % comment
% lex([$%|T]	,Acc,{Line,Row} = LnRw)->
% 	nextline(T,Acc,LnRw);

% erlang direct
lex([$%,$%|T],Acc,{Line,Row} = LnRw)->
	{Rest,Token,NewLnRw} = lex_direct(T,[],LnRw ),
	String = eosstd:to_bin(Token),
	lex(Rest,[?token(LnRw,erlang_direct,String)|Acc],NewLnRw);

% // comment
lex([$/,$/|T]	,Acc,{Line,Row} = LnRw)->
	nextline(T,Acc,LnRw);

% /* comment */
lex([$/,$*|T]	,Acc,{Line,Row} = LnRw)->
	skipcomment(T,Acc,LnRw);

% formatted / binary string
lex([$@,$'|T],Acc,{Line,Row} = LnRw)->
	{Rest,Token,NewLnRw} = lex_string2(T,[],LnRw ),
	String = eosstd:to_bin(Token),
	lex(Rest,[?token(LnRw,fmt_binarystring,String)|Acc],NewLnRw);

% formatted / string list
lex([$@,$"|T],Acc,{Line,Row} = LnRw)->
	{Rest,Token,NewLnRw} = lex_string(T,[],LnRw ),
	String = eosstd:to_bin(Token),
	lex(Rest,[?token(LnRw,fmt_stringlist,String)|Acc],NewLnRw);


% here document
% <<<SYMBOL
% SYMBBOL
lex([$<,$<,$<|T],Acc,{Line,Row} = LnRw)->
	{Rest,SYMBOL,NewLnRw} = until_eol(T,[],LnRw ),
	{Rest2,DOC,NewLnRw2}=until_symbol(Rest,[],NewLnRw,SYMBOL),
	lex(Rest2,[?token(LnRw,binarystring,escape(DOC))|Acc],NewLnRw2);


% binary string
lex([$<,$<,$"|T],Acc,{Line,Row} = LnRw)->
	{[$>,$>|Rest],Token,NewLnRw} = lex_string(T,[],LnRw ),
	String = eosstd:to_bin(Token),
	lex(Rest,[?token(LnRw,binarystring,String)|Acc],NewLnRw);

% binary string
lex([$'|T],Acc,{Line,Row} = LnRw)->
	{Rest,Token,NewLnRw} = lex_string2(T,[],LnRw ),
	String = eosstd:to_bin(Token),
	lex(Rest,[?token(LnRw,binarystring,String)|Acc],NewLnRw);

% string list
lex([$"|T],Acc,{Line,Row} = LnRw)->
	{Rest,Token,NewLnRw} = lex_string(T,[],LnRw ),
	String = eosstd:to_bin(Token),
	lex(Rest,[?token(LnRw,stringlist,String)|Acc],NewLnRw);




% symbol/atom
lex([$:,$'|T],Acc,{Line,Row} = LnRw)->
	{Rest,Token,NewLnRw} = lex_string2(T,[],LnRw ),
	lex(Rest,[?token(LnRw,quotedsymbol,eosstd:to_atom(Token))|Acc],NewLnRw);

% character
lex([$\\,$'|T],Acc,{Line,Row} = LnRw)->
	{Rest,Token,NewLnRw} = lex_string2(T,[],LnRw ),
	%1 == length(Token),
	%[Code|_] = Token,
	%Chars = unicode:characters_to_list( binary_to_list(to_bin(Token))),
	Chars = Token,
	lex(Rest,[?token(LnRw,char,Chars )|Acc],NewLnRw);


%
lex([$<,$< |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'<<')|Acc],{Line,Row+2} );
lex([$>,$> |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'>>')|Acc],{Line,Row+2});



lex([$<,$<,$- |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'<<-')|Acc],{Line,Row+3} );

lex([$-,$> |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'->')|Acc],{Line,Row+2} );
lex([$<,$- |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'<-')|Acc],{Line,Row+2} );

lex([$:,$= |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,':=')|Acc],{Line,Row+2} );

lex([$+,$+ |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'++')|Acc],{Line,Row+1} );

lex([$& |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'&')|Acc],{Line,Row+1} );

lex([$:,$: |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'::')|Acc],{Line,Row+2} );


%%lex([$: |T],Acc,{Line,Row} = LnRw)->
%%	lex(T,[?token(LnRw,':')|Acc],{Line,Row+1} );

lex([$. |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'.')|Acc],{Line,Row+1} );
lex([$+ |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'+')|Acc],{Line,Row+1} );
lex([$- |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'-')|Acc],{Line,Row+1} );
lex([$* |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'*')|Acc],{Line,Row+1} );
lex([$/ |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'/')|Acc],{Line,Row+1} );


lex([$|,$> |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'|>')|Acc],{Line,Row+2} );
lex([$<,$| |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'<|')|Acc],{Line,Row+2} );


lex([$=,$=,$= |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'=:=')|Acc],{Line,Row+3} );

lex([$!,$=,$= |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'=/=')|Acc],{Line,Row+3} );

lex([$!,$= |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'/=')|Acc],{Line,Row+2} );

lex([$=,$= |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'==')|Acc],{Line,Row+2} );
%lex([$=,$> |T],Acc,{Line,Row} = LnRw)->
%	lex(T,[?token(LnRw,bop,'=>')|Acc],{Line,Row+2} );
lex([$<,$= |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'=<')|Acc],{Line,Row+2} );
lex([$>,$= |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'>=')|Acc],{Line,Row+2} );
lex([$< |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'<')|Acc],{Line,Row+1} );
lex([$> |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'>')|Acc],{Line,Row+1} );
lex([$= |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'=')|Acc],{Line,Row+1} );


lex([$(,$\\ |T],Acc,{Line,Row} = LnRw)->
	lex(T,[ ?token(LnRw,'lambda')|Acc],{Line,Row+2} );

lex([$( |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'(')|Acc],{Line,Row+1} );
lex([$) |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,')')|Acc],{Line,Row+1} );


lex([$[ |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'[')|Acc],{Line,Row+1} );
lex([$] |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,']')|Acc],{Line,Row+1} );

lex([${ |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'{')|Acc],{Line,Row+1} );
lex([$} |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'}')|Acc],{Line,Row+1});

lex([${ |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'{')|Acc],{Line,Row+1} );
lex([$} |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'}')|Acc],{Line,Row+1});


lex([$|,$| |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'||')|Acc],{Line,Row+2});
lex([$| |T],Acc,{Line,Row} = LnRw)->
	lex(T,[?token(LnRw,'|')|Acc],{Line,Row+1});

lex([$# |T],Acc,{Line,Row} = LnRw)->
	%lex(T,[?token(LnRw,'#')|Acc],{Line,Row+1});
	{Rest,X,NewLnRw } = lex_symbol(T,LnRw ),
	lex(Rest,[?token(LnRw,record_name,X)|Acc],NewLnRw);	


lex([H|T] = Src,Acc,{Line,Row}=LnRw) ->
	%io:format("- lex ~p : ~p~n",[Src,Acc]),
	IsDigit = is_digit(H),
	IsUpper = is_u_alpha(H), 
	if	IsDigit ->	{Rest,XToken,NewLnRw } = lex_number(Src,LnRw ),
					case XToken of
						{'float',X} ->		lex(Rest,[?token(LnRw,number,X)|Acc],NewLnRw);
						{'int',X} ->		lex(Rest,[?token(LnRw,number,X)|Acc],NewLnRw);
						{'n_int',N,X} ->	lex(Rest,[?token(LnRw,n_int,{N,X})|Acc],NewLnRw);
						{'e_int',X,N} ->	lex(Rest,[?token(LnRw,e_int,{X,N})|Acc],NewLnRw)
					end;

		H == $$ ->	{Rest,X,NewLnRw } = lex_variable(T,LnRw ),
					lex(Rest,[?token(LnRw,globalvar,X)|Acc],NewLnRw);
		H == $@ ->	{Rest,X,NewLnRw } = lex_variable(T,LnRw ),
					lex(Rest,[?token(LnRw,membervar,X)|Acc],NewLnRw);
		H == $? ->	{Rest,X,NewLnRw } = lex_variable(Src,LnRw ),
					lex(Rest,[?token(LnRw,var,X)|Acc],NewLnRw);
		IsUpper ->	{Rest,X,NewLnRw } = lex_variable(Src,LnRw ),
					lex(Rest,[?token(LnRw,var,X)|Acc],NewLnRw);
		H == $: ->	{Rest,X,NewLnRw } = lex_symbol_w(T,LnRw ),
					lex(Rest,[?token(LnRw,quotedsymbol,X)|Acc],NewLnRw);
		% symbol/atom
		true ->		{Rest,X,NewLnRw } = lex_symbol(Src,LnRw ),%io:format("sym = ~p : ~p~n",[X,Rest]),
					if X == '' ->
						[SkipH|SkipRest] = Rest,
						%%io:format("skip char: ~c\n",[SkipH]),
						erleos:compile_error(lexer,NewLnRw,eosstd:fmt("invalid char ~c",[SkipH])),
						lex(SkipRest,Acc,NewLnRw);
					true ->
	                    Symobj = case erleos_keyword:is_keyword(X) of
	                    	true -> X;
		                    false -> []
	                    end,
	                    case Symobj of
	                    	[] ->
								lex(Rest,[?token(LnRw,symbol,X)|Acc],NewLnRw);
							_ ->
								lex(Rest,[?token(LnRw,Symobj)|Acc],NewLnRw)
						end
					end
	end.

% {Line,Row}
% token { {Line,Row},TokenData }
lex(Src) ->
	%lex( to_list(Src),[],{0,0} ).
	lex( Src,[],{0,0} ).

%

test()->
	[{{0,0},{symbol,'abc/1'}}] = lex("abc/1"),
	[{{0,0},{var,'Abc'}},{{0,3},{bop,'/'}},{{0,4},{number,1}}] = lex("Abc/1").
