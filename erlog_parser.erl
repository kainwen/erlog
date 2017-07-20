-module(erlog_parser).

-include_lib("eunit/include/eunit.hrl").

-export([scan_and_parse/1, scan_and_parse_file/1]).

-export_type([rule/0, program/0, predicate/0, symbol/0]).


-type rule() :: {fact, head()}
	      | {rule, head(), body()}.

-type head() :: predicate().

-type body() :: [predicate()].

-type const() :: {const, atom()}.

-type var() :: {var, atom()}.

-type symbol() :: const() | var().

-type predicate() :: {{atom(), integer()}, [symbol()]}.

-type program() :: [rule()].

-type token() :: '('
	       | ')'
	       | ','
	       | '.'
	       | '<-'
	       | {id, atom()}.

-type delim() :: token()
	       | nil.

%%%%%%%%%%%%%%%%%%%%%%%%%%API%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec scan_and_parse(string()) -> program().
scan_and_parse(Code) ->
    {ok, Toks, _} = erlog_tok:string(Code),
    parse_program(Toks).

-spec scan_and_parse_file(string()) -> program().
scan_and_parse_file(Fn) ->
    {ok, Data} = file:read_file(Fn),
    Code = binary_to_list(Data),
    scan_and_parse(Code).

-spec parse_program([token()]) -> program().
parse_program(Toks) ->
    {Rules, []} = parse_multi(fun parse_rule/1, Toks, nil),
    Rules.

%%%%%%%%%%%%%%%%%%%%%%Parse syntax%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec parse_symbol([token()]) -> {symbol(), [token()]}.
parse_symbol([{id, S}|Toks]) when is_atom(S) ->
    case is_var(S) of
	true ->
	    {{var, S}, Toks};
	false ->
	    {{const, S}, Toks}
    end.

-spec parse_head([token()]) -> {predicate(), [token()]}.
parse_head([{id, P}, '('|Toks]) when is_atom(P) ->
    assert(is_var(P), false),
    {Args, [')'|R1]} = parse_multi(fun parse_symbol/1, Toks, ','),
    {{{P, length(Args)}, Args}, R1}.

-spec parse_body([token()]) -> {body(), [token()]}.
parse_body(Toks) ->
    parse_multi(fun parse_head/1, Toks, ',').

-spec parse_rule([token()]) -> {rule(), [token()]}.
parse_rule(Toks) ->
    {Head, R1} = parse_head(Toks),
    case R1 of
	['.'|R2] ->
	    {{fact, Head}, R2};
	['<-'|R2] ->
	    {Body, ['.'|R3]} = parse_body(R2),
	    {{rule, Head, Body}, R3}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%Parser Combinators%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec parse_multi(fun (([token()]) -> {Term::T, [token()]}), [token()], delim()) -> {[T], [token()]}.
parse_multi(Parse_func, Toks, Delim) ->
    parse_multi_helper(Parse_func, Toks, Delim, []).

-spec parse_multi_helper(fun (([token()]) -> {Term::T, [token()]}), [token()], delim(), [T]) -> {[T], [token()]}.
parse_multi_helper(Parse_func, Toks, Delim, Acclist) ->
    try Parse_func(Toks) of
	{Term, R} ->
	    case {R, Delim} of
		{[], _} -> 
		    {lists:reverse([Term|Acclist]), []};
		{_, nil} ->
		    parse_multi_helper(Parse_func, R, Delim, [Term|Acclist]);
		{[D|R1], Delim} when D =:= Delim ->
		    parse_multi_helper(Parse_func, R1, Delim, [Term|Acclist]);
		_ ->
		    {lists:reverse([Term|Acclist]), R}
	    end
    catch _:_ ->
	    {lists:reverse(Acclist), Toks}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%Internal Functions%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec is_var(atom()) -> boolean().
is_var(S) ->
    Ch = lists:nth(1, atom_to_list(S)),
    (Ch >= 65) and (90 >= Ch).

-spec assert(any(), any()) -> any().
assert(A, B) ->
    A = B.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%Unit Test%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_multi_test() ->
    Toks1 = [a, b, c, 1],
    F1 = fun ([T|Ts]) when is_atom(T) ->
		 {T, Ts}
	 end,
    ?assert(parse_multi(F1, Toks1, nil) =:= {[a,b,c], [1]}),
    Toks2 = [a, ',', b, ',', c, ',', d, ')', 1, 2],
    ?assert(parse_multi(F1, Toks2, ',') =:= {[a,b,c,d], [')',1,2]}),
    Toks3 = [{id, a}, {id, 'X'}, {id, c}, 1, 2, 3],
    ?assert(parse_multi(fun parse_symbol/1, Toks3, nil) =:=
	    {[{const, a}, {var, 'X'}, {const, c}], [1,2,3]}).

parse_head_test() ->
    Toks = [{id, p}, '(', {id, x}, ',', {id, 'Y'}, ')'],
    ?assert(parse_head(Toks) =:= {{{p, 2}, [{const, x}, {var, 'Y'}]}, []}).

parse_rule_test() ->
    Code = "p(X) <- a(X), b(X, Y), c(d).",
    {ok, Toks, _} = erlog_tok:string(Code),
    ?assert(parse_rule(Toks) =:=
		{{rule,
		  {{p,1}, [{var, 'X'}]},
		  [{{a,1}, [{var, 'X'}]},
		   {{b,2}, [{var, 'X'}, {var, 'Y'}]},
		   {{c,1}, [{const, d}]}]},
		 []}).
