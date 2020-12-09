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
