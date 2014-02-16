-module(erleos_gensym).
-behaviour(gen_server).

-record(state,{prefix,count}).

%-export([start_link/1]).
%-export([alloc/0]).
%-export([init/1, handle_call/3]).
-compile(export_all).

start() ->
	case whereis(?MODULE) of
	undefined ->
		gen_server:start({local, ?MODULE}, ?MODULE, [], []);
	_ -> ok
	end.

alloc(Prefix) ->
	gen_server:call(?MODULE, {alloc,Prefix}).

init([]) ->
	{ok, #state{prefix="@t_",count=0}}.

%

handle_call({alloc,Prefix}, _From, State) ->
	NewSymStr = io_lib:format("~s~s~p", [Prefix,State#state.prefix,State#state.count]),
	NewSym = list_to_atom( lists:flatten(NewSymStr) ),
    {reply, NewSym,State#state{count = State#state.count+1 } };

handle_call(Message, _From, State) ->
	io:format("unknown call: ~p~n",[Message]),
	{reply, error, State}.

% We get compile warnings from gen_server unless we define these
handle_cast(Message, State) ->
	io:format("unknown cast: ~p~n",[Message]),
	{noreply, State}.
handle_info(Message, State) ->
	io:format("unknown info: ~p~n",[Message]),
	{noreply, State}.
terminate(Reason, _Library) ->
	io:format("terminate: ~p~n",[Reason]),
	ok.
code_change(_OldVersion, State, _Extra) ->
	io:format("codechange: ~n",[]),
	{ok, State}.