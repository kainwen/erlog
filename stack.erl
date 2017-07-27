-module(stack).

-export([new/0, delete/1, loop/1, push/2, pop/1, is_empty/1]).

new() ->
    spawn(?MODULE, loop, [[]]).

delete(Stk) ->
    Stk ! {stop, self()}.

push(Stk, E) ->
    Stk ! {{push, E}, self()},
    receive
	ok -> ok
    end.

is_empty(Stk) ->
    Stk ! {{is_empty}, self()},
    receive
	B -> B
    end.

pop(Stk) ->
    Stk ! {{pop}, self()},
    receive
	{ok, E} ->
	    E;
	{error, Msg} -> erlang:error(Msg)
    end.	    

loop(Stk) ->
    receive
	{{is_empty}, From} ->
	    Res = case length(Stk) of
		      0 -> true;
		      N when N > 0 -> false
		  end,
	    From ! Res,
	    loop(Stk);
	{{pop}, From} ->
	    case length(Stk) of
		0 ->
		    From ! {error, "pop empty stack"},
		    loop(Stk);
		N when N > 0 ->
		    [E|Rem_stack] = Stk,
		    From ! {ok, E},
		    loop(Rem_stack)
	    end;
	{{push, E}, From} ->
	    From ! ok,
	    loop([E|Stk]);
	{stop, _From} ->
	    ok;
	_ ->
	    loop(Stk)
    end.
		
