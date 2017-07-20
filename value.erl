-module(value).

-behaviour(gen_server).

-export([new/1, get/1, set/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%API%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new(V) ->
    {ok, Value} = gen_server:start_link(?MODULE, V, []),
    Value.

get(V) ->
    gen_server:call(V, {get}).

set(V, New) ->
    gen_server:call(V, {set, New}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Callbacks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(V) ->
    {ok, V}.

handle_call({get}, _From, V) ->
    {reply, V, V};
handle_call({set, New}, _From, _V) ->
    {reply, ok, New}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

