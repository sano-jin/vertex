#!/bin/bash
i=0

test() {
    input="$2"
    testName="$1"
    id=`printf %02d $i`
    DIR="test-${id}-${testName}"
    mkdir $DIR
    echo "$input" > "${DIR}/input.dhl"
    echo $DIR
    echo "$input"
    stack exec dhli -- "${DIR}/input.dhl" >  "${DIR}/output.log"
    echo ""
    i=$((i+1))
    echo $i
}

test single_a                "a"
test zero_arity              "a()"
test embedded_atoms          "a(b, c)"
test multiset_of_2           "a, d"
test multiset_of_4           "d, c, b, a"
test multiset_in_parenthesis "d, ((c, b), a)"
test locallink_zero_indeg    "\A.A -> a"
test locallink_1_indeg       "\A.\C.(A -> a(C, C), C -> g)"
test a_rule_a2b              "a :- b"
test null_rhs                "a :- . b"
# test null_lhs              ":- a. b"
test rule_on_rhs             "a. a :- b, (b :- c)"
test locallink_on_lhs1       "\X Y.(a, c(d)) :- b, c . d."
test locallink_on_lhs2       "\X.\Y.(X -> a(X), c(d(X))) :- b, c . d."
test append                  \
"append(cons(a, cons(b, nil)), cons(c, nil)).
R -> append(cons(H, T), L) :- R -> cons(H, append(T, L)).
R -> append(nil, L) :- R -> L."
test append2                  \
"append([a, b], [c]).
R -> append([H|T], L) :- R -> [H|append(T, L)].
R -> append([], L) :- R -> L."
test processContext1        \
"a(\$p) :- \$p' := \$p + 1 | b(\$p'). a(1)."
test intIsUnary "a(3). a(\$p:unary) :- \$q := \$p + 1 | b(\$q)"
test noMultipleUntypedProcessContext "a(\$p), b(\$q:string), c(\$r) :- \$q = \$p |."
test typeCheckOnGuard "a(\"hoge\"). a(\$p) :- \$p:string | b."
test eqInt "a(1, 2). a(\$p, \$q) :- \$p = \$q | b."
test neqInt "a(1, 2). a(\$p, \$q) :- \$p /= \$q | b."
test add "a(1, 2). a(\$p, \$q) :- \$r := \$p + \$q | b(\$r)."
test sub "a(3, 1). a(\$p, \$q) :- \$r := \$p - \$q | b(\$r)."
test mul "a(2, 3). a(\$p, \$q) :- \$r := \$p * \$q | b(\$r)."
test product \
"n(1), n(2), n(3), n(4), n(5), n(6).
n(\$x), n(\$y) :- \$z := \$x * \$y | n(\$z).
"
test bubbleSort \
"L -> [\$x,\$y|L2] :- \$x > \$y | L->[\$y,\$x|L2].
res([3,1,7,0,5,4,2])."
test fl_interpreter \
"result(eval(
	let_rec(\"fac\",
		\"n\",
		if(\"n\" < 1, 1, \"n\" * app(\"fac\", \"n\" - 1)),
		app(\"fac\", 5)
	),
	[]
)).

let_rec @@
R -> eval(let_rec(\$var:string, V, E1, E2), Env)
:- \Env'.(
   R -> eval(E2, Env'),
   Env' -> [asc(\$var, val(cl(V, E1, Env')))|Env]
).


eval_if @@
R -> eval(if(E1, E2, E3), Env)
:- R -> if(eval(E1, Env), E2, E3, Env).



let_abbreviation @@
R -> let([X|Xs], E1, E2)
:- R -> app(lam(X, E2), fun(Xs, E1)).

function_abbreviation @@
R -> fun([X|Xs], Body)
:- R -> lam(X, fun(Xs, Body)).

nullary_function @@
R -> fun([], Body)
:- R -> Body.

beta_reduction @@
R -> app(val(cl(X, Body, Env)), val(E2))
:- R -> eval(Body, [asc(X, val(E2))|Env]).


eval_application @@
R -> eval(app(E1, E2), Env)
:- R -> app(eval(E1, Env), eval(E2, Env)).

eval_abstraction @@
R -> eval(lam(X, Body), Env)
:- R -> val(cl(X, Body, Env)).

eval_int @@
R -> eval(\$p:int, Env)
:- R -> val(\$p).


% built-in functions
eval_plus @@
R -> eval(E1 + E2, Env)
:- R -> eval(E1, Env) + eval(E2, Env).

add @@
R -> val(\$p) + val(\$q)
:- \$r := \$p + \$q
|  R -> val(\$r).


eval_minus @@
R -> eval(E1 - E2, Env)
:- R -> eval(E1, Env) - eval(E2, Env).

sub @@
R -> val(\$p) - val(\$q)
:- \$r := \$p - \$q
|  R -> val(\$r).


eval_times @@
R -> eval(E1 * E2, Env)
:- R -> eval(E1, Env) * eval(E2, Env).

mul @@
R -> val(\$p) * val(\$q)
:- \$r := \$p * \$q
|  R -> val(\$r).

eval_true @@
R -> eval(true, Env)
:- R -> val(true).

eval_false @@
R -> eval(false, Env)
:- R -> val(false).


eval_less @@
R -> eval(E1 < E2, Env)
:- R -> eval(E1, Env) < eval(E2, Env).


less_true @@
R -> val(\$p) < val(\$q)
:- \$p < \$q
|  R -> val(true).

less_false @@
R -> val(\$p) < val(\$q)
:- \$p >= \$q
|  R -> val(false).


if_true @@
R -> if(val(true), E1, E2, Env)
:- R -> eval(E1, Env).

if_false @@
R -> if(val(false), E1, E2, Env)
:- R -> eval(E2, Env).

lookup @@
R -> eval(\$var1:string, Env), Env -> [asc(\$var2:string, Val)|T]
:- \$var1 /= \$var2
|  R -> eval(\$var1, T), Env -> [asc(\$var2, Val)|T].

resolve @@
R -> eval(\$var1:string, Env), Env -> [asc(\$var2:string, val(Val))|T]
:- \$var1 = \$var2
|  R -> val(cp(Val)), Env -> [asc(\$var2, val(Val))|T].


% copying
copy_value @@
R -> cp(Val), Val -> \$p:int
:- R -> \$p, Val -> \$p.

copy_variable @@
R -> cp(Var), Var -> \$p:string
:- R -> \$p, Var -> \$p.

copy_application @@
R -> cp(App), App -> app(E1, E2)
:- R -> app(cp(E1), cp(E2)), App -> app(E1, E2).

copy_closure @@
R -> cp(Cl), Cl -> cl(\$x:string, Body, Env)
:- R -> cl(\$x, cp(Body), Env), Cl -> cl(\$x, Body, Env).

copy_abstraction @@
R -> cp(Lam), Lam -> lam(\$x:string, Body)
:- R -> lam(\$x, cp(Body)), Lam -> lam(\$x, Body).

copy_plus @@
R -> cp(Plus), Plus -> E1 + E2
:- R -> cp(E1) + cp(E2), Plus -> E1 + E2.


copy_times @@
R -> cp(Times), Times -> E1 * E2
:- R -> cp(E1) * cp(E2), Times -> E1 * E2.

copy_minus @@
R -> cp(Minus), Minus -> E1 - E2
:- R -> cp(E1) - cp(E2), Minus -> E1 - E2.

copy_less @@
R -> cp(Less), Less -> E1 < E2
:- R -> cp(E1) < cp(E2), Less -> E1 < E2.



copy_cons @@
R -> cp(Cons), Cons -> [H|T]
:- R -> [cp(H)|cp(T)], Cons -> [H|T].

copy_if @@
R -> cp(If), If -> if(E1, E2, E3)
:- R -> if(cp(E1), cp(E2), cp(E3)), If -> if(E1, E2, E3).

% Garbage collection

gc_cons @@
[asc(\$var:string, val(Val))|T] :- .

gc_cons_loop @@
\Env.Env -> [asc(\$var:string, val(cl(\$var2:string, Body, Env)))|T] :- .

gc_nil @@
[] :- .

gc_int @@
\$p:int :- .

gc_val @@
val(Val) :- .

gc_let @@
let(\$var, E1, E2) :- .

gc_closure @@
cl(\$var:string, E1, Env) :- .

gc_lambda @@
lam(\$var:string, Body) :- .

gc_app @@
app(E1, E2) :-.

gc_plus @@
E1 + E2 :-.

gc_minus @@
E1 - E2 :-.

gc_times @@
E1 * E2 :-.

gc_less @@
E1 < E2 :-.

gc_variable @@
\$var:string :- .

gc_if @@
if(E1, E2, E3) :- .

"


