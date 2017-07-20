-module(env).

-behaviour(gen_server).

-export_type([env/0]).

-export([new/0, apply_env/2, extend_env/3, link_vars/3, delete/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type env() :: pid().

%%%%%%%%%%%%%%%%%%%%%%%%%%%APIs%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new() ->
    {ok, Env} = gen_server:start_link(?MODULE, [], []),
    Env.

apply_env(Env, V) ->
    gen_server:call(Env, {apply_env, V}).

extend_env(Env, V, C) ->
    gen_server:call(Env, {extend_env, V, C}).

link_vars(Env, V1, V2) ->
    gen_server:call(Env, {link_vars, V1, V2}).

delete(Env) ->
    gen_server:stop(Env).

%%%%%%%%%%%%%%%%%%%%%%%%%Callbacks%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, []}.

handle_call({apply_env, V}, _From, Env) ->
    case proplists:get_value(V, Env) of
	undefined ->
	    {reply, {fail}, Env};
	Value ->
	    case value:get(Value) of
		{nil} ->
		    {reply, {fail}, Env};
		{c, C} ->
		    {reply, {ok, C}, Env}
	    end
    end;
handle_call({extend_env, V, C}, _From, Env) ->
    case proplists:get_value(V, Env) of
	undefined ->
	    Value = value:new({c, C}),
	    New_env = [{V, Value}|Env],
	    {reply, ok, New_env};
	Value ->
	    {nil} = value:get(Value),
	    ok = value:set(Value, {c, C}),
	    {reply, ok, Env}
    end;
handle_call({link_vars, V1, V2}, _From, Env) ->
    Value = value:new({nil}),
    New_env = [{V1, Value}, {V2, Value}|Env],
    {reply, ok, New_env}.


handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

