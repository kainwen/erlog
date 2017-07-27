-module(dependency_graph).

-export([get_all_derived_predicate/1,
	 build_dependency_graph/1,
	 is_recursive/1,
	 is_mutual_recursive/3,
	 is_depend_on/3,
	 leq/2]).

get_all_derived_predicate([]) -> [];
get_all_derived_predicate([{fact, _}|Dl_program]) ->
    get_all_derived_predicate(Dl_program);
get_all_derived_predicate([{rule, Head, _Body}|Dl_program]) ->
    [Head | get_all_derived_predicate(Dl_program)].

build_dependency_graph(Dl_program) ->
    Derived_predicates = get_all_derived_predicate(Dl_program),
    D = dict:from_list([{P, 1}
			|| {P, _} <- Derived_predicates]),
    G = digraph:new(),
    [digraph:add_vertex(G, P)
     || {P, _} <- Derived_predicates],
    add_edges(Dl_program, D, G).

add_edges([], _D, G) -> G;
add_edges([{fact, _}|Dl_program], D, G) -> 
    add_edges(Dl_program, D, G);
add_edges([{rule, Head, Body}|Dl_program], D, G) -> 
    add_edges(Head, Body, D, G),
    add_edges(Dl_program, D, G).

add_edges(_Head, [], _D, G) -> G;
add_edges(Head={P1, _}, [{P2, _}|Body], D, G) -> 
    case dict:is_key(P2, D) of
	true ->
	    digraph:add_edge(G, P2, P1),
	    add_edges(Head, Body, D, G);
	false ->
	    add_edges(Head, Body, D, G)
    end.

is_recursive(G) ->
    lists:any(fun (V) ->
		      case digraph:get_cycle(G, V) of
			  false -> false;
			  _ -> true
		      end
	      end,
	      digraph:vertices(G)).

is_mutual_recursive(G, P1, P2) ->
    lists:any(fun (V) ->
		      case digraph:get_cycle(G, V) of
			  false ->
			      false;
			  Vs ->
			      S = sets:from_list(Vs),
			      sets:is_element(P1, S) and
				  sets:is_element(P2, S)
		      end
	      end,
	      digraph:vertices(G)).


is_depend_on(G, P1, P2) ->
    case digraph:get_path(G, P2, P1) of
	false -> false;
	_ -> true
    end.
	    
leq(G, P) ->
    ets:new(visited, [named_table]),
    Stk = stack:new(),
    stack:push(Stk, P),
    ets:insert(visited, {P, 1}),
    Res = leq_loop(Stk, G),
    ets:delete(visited),
    stack:delete(Stk),
    Res.

leq_loop(Stk, G) ->
    case stack:is_empty(Stk) of
	true -> 
	    ets:foldl(fun ({K, _}, Acc) -> [K|Acc] end, [], visited);
	false ->
	    P = stack:pop(Stk),
	    Vs = digraph:out_neighbours(G, P),
	    [case ets:member(visited, V) of
		 false ->
		     stack:push(Stk, V),
		     ets:insert(visited, {V, 1});
		 true -> ok
	     end
	     || V <- Vs],
	    leq_loop(Stk, G)
    end.
