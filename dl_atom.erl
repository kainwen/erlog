-module(dl_atom).

-include_lib("eunit/include/eunit.hrl").

-export([is_ground/1, is_instance_atom/3, is_instance_rule/3]).

-export_type([dl_atom/0]).

-type dl_atom() :: erlog_parser:predicate().
-type symbol() :: erlog_parser:symbol().
-type rule() :: erlog_parser:rule().

-spec is_ground(erlog_parser:predicate()) -> boolean().
is_ground({_Predicate, Args}) ->
    lists:all(fun (Arg) ->
		      case Arg of
			  {var, _} -> false;
			  {const, _} -> true
		      end
	      end,
	      Args).
-spec is_instance_atom(dl_atom(), dl_atom(), env:env()) -> boolean().
is_instance_atom({P1, _Args1}, {P2, _Args2}, _Env) when P1 /= P2 -> false;
is_instance_atom({_P1, Args1}, {_P2, Args2}, Env) ->
    is_instance_atom_with_env(Args1, Args2, Env).


-spec is_instance_atom_with_env([symbol()], [symbol()], env:env()) -> boolean().
is_instance_atom_with_env([], [], _Env) -> true;
is_instance_atom_with_env([], [_A|_Args], _Env) -> false;
is_instance_atom_with_env([_A|_Args], [], _Env) -> false;
is_instance_atom_with_env([A|Args1], [B|Args2], Env) ->
    case {A, B} of
	{{const, C1}, {const, C2}} when C1 =:= C2 ->
	    is_instance_atom_with_env(Args1, Args2, Env);
	{{const, _}, {const, _}} ->
	    false;
	{{const, C1}, {var, V2}} ->
	    case env:apply_env(Env, V2) of
		{ok, C} when C =:= C1 ->
		    is_instance_atom_with_env(Args1, Args2, Env);
		{ok, C} when C /= C1 ->
		    false;
		{fail} ->
		    ok = env:extend_env(Env, V2, C1),
		    is_instance_atom_with_env(Args1, Args2, Env)
	    end;
	{{var, V1}, {const, C2}} ->
	    case env:apply_env(Env, V1) of
		{ok, C} when C =:= C2 ->
		    is_instance_atom_with_env(Args1, Args2, Env);
		{ok, C} when C /= C2 ->
		    false;
		{fail} ->
		    ok = env:extend_env(Env, V1, C2),
		    is_instance_atom_with_env(Args1, Args2, Env)
	    end;
	{{var, V1}, {var, V2}} when V1 =:= V2 ->
	    is_instance_atom_with_env(Args1, Args2, Env);
	{{var, V1}, {var, V2}} when V1 /= V2 ->
	    case {env:apply_env(Env, V1), env:apply_env(Env, V2)} of
		{{fail}, {fail}} ->
		    ok = env:link_vars(Env, V1, V2),
		    is_instance_atom_with_env(Args1, Args2, Env);
		{{fail}, {const, C}} ->
		    ok = env:extend_env(Env, V1, C),
		    is_instance_atom_with_env(Args1, Args2, Env);
		{{const, C}, {fail}} ->
		    ok = env:extend_env(Env, V2, C),
		    is_instance_atom_with_env(Args1, Args2, Env);
		{{const, C1}, {const, C2}} when C1 =:= C2 ->
		    is_instance_atom_with_env(Args1, Args2, Env);
		_ -> false
	    end
    end.

-spec is_instance_rule(rule(), rule(), env:env()) -> boolean().
is_instance_rule({fact, Head1}, {fact, Head2}, Env) ->
    is_instance_atom(Head1, Head2, Env);
is_instance_rule({fact, _}, {rule, _}, _E) -> {false};
is_instance_rule({rule, _}, {fact, _}, _E) ->  {false};
is_instance_rule({rule, Head1, Body1}, {rule, Head2, Body2}, Env) ->
    case is_instance_atom(Head1, Head2, Env) of
	false ->
	    false;
	true when length(Body1) /= length(Body2) ->
	    false;
	true ->
	    lists:all(fun ({P1, P2}) -> 
			      is_instance_atom(P1, P2, Env)
		      end,
		      lists:zip(Body1, Body2))
    end.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Unit Test%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_ground_test() ->
    P1 = {{p, 1}, [{const, a}]},
    ?assert(is_ground(P1) =:= true),
    P2 = {{p, 2}, [{const, a}, {var, b}]},
    ?assert(is_ground(P2) =:= false).
    
    
is_instance_atom_test() ->
    P1 = {{p, 1}, [{const, a}]},
    P2 = {{p, 1}, [{var, 'X'}]},
    P3 = {{p, 1}, [{const, b}]},
    P4 = {{p, 3}, [{const, a}, {const, b}, {const ,c}]},
    P5 = {{p, 3}, [{const, a}, {const, b}, {const ,a}]},
    P6 = {{p, 3}, [{var, 'X'}, {var, 'Y'}, {var ,'X'}]},
    P7 = {{p, 3}, [{var, 'X'}, {const, c}, {var ,'X'}]},
    ?assert(is_instance_atom(P1, P2, env:new()) =:= true),
    ?assert(is_instance_atom(P1, P3, env:new()) =:= false),
    ?assert(is_instance_atom(P4, P5, env:new()) =:= false),
    ?assert(is_instance_atom(P4, P6, env:new()) =:= false),
    ?assert(is_instance_atom(P5, P6, env:new()) =:= true),
    ?assert(is_instance_atom(P6, P6, env:new()) =:= true),
    ?assert(is_instance_atom(P6, P7, env:new()) =:= true).
    

is_instance_rule_test() ->
    R1 = {fact, {{p, 2}, [{const, a}, {const, a}]}},
    R2 = {fact, {{p, 2}, [{var, 'X'}, {const, a}]}},
    ?assert(is_instance_rule(R1, R2, env:new()) =:= true),
    R3 = {rule,
	  {{p, 1}, [{var, 'X'}]},
	  [{{f, 2}, [{var, 'X'}, {var, 'Y'}]},
	   {{g, 1}, [{var, 'Y'}]},
	   {{h, 2}, [{var, 'Z'}, {const, a}]}]},
    R4 = {rule,
	  {{p, 1}, [{var, 'X'}]},
	  [{{f, 2}, [{const, x}, {var, 'Y'}]},
	   {{g, 1}, [{var, 'Y'}]},
	   {{h, 2}, [{var, 'Z'}, {var, 'X'}]}]},
    R5 = {rule,
	  {{p, 1}, [{var, 'X'}]},
	  [{{f, 2}, [{var, 'X'}, {var, 'Y'}]},
	   {{g, 1}, [{var, 'Y'}]},
	   {{h, 2}, [{var, 'Z'}, {const, x}]}]},
    ?assert(is_instance_rule(R3, R4, env:new()) =:= false),
    ?assert(is_instance_rule(R4, R5, env:new()) =:= true).
