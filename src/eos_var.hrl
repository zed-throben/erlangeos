
remove_last([H],Acc)->
	{H,lists:reverse(Acc)};
remove_last([H|T],Acc)->
	remove_last(T,[H|Acc]).

remove_last([])->
	{[],[]};
remove_last(Src)->
	remove_last(Src,[]).

%

modify(Key,Fn)->
	NewValue = Fn(get(Key)), 
	put(Key,NewValue),
	NewValue.

%

lget(Key)->
	case get(Key) of
		undefined -> [];
		X -> X
	end.

num_get(Key)->
	case get(Key) of
		undefined -> 0;
		X -> X
	end.

%

lpush(Key,Value)->
	put(Key,lget(Key)++[Value]).

lpop(Key)->
	{Last,Remains} = remove_last(lget(Key)),
	put(Key,Remains),
	Last.

%

lunshift(Key,Value)->
	put(Key,[Value|lget(Key)]).

lshift(Key)->
	case lget(Key) of
		[H] -> T = [];
		[H|T] -> [];
		[] ->	H = [],
				T = []
	end,
	put(Key,T),
	H.

%

ltop(Key)->
	case lget(Key) of
		[H|_] -> H;
		_ -> []
	end.

llast(Key)->
	case lget(Key) of
		[] -> [];
		List -> lists:last(List)
	end.


%

inc(Key)->
	N = num_get(Key) + 1,
	put( Key,N ),
	N.

dec(Key)->
	N = num_get(Key) - 1,
	put( Key,N ),
	N.

add(Key,Value)->
	N = num_get(Key) + Value,
	put( Key,N ),
	N.

