#!/bin/bash
i=0

test() {
    input="$2"
    testName="$1"
    id=`printf %02d $i`
    DIR="test-${id}-${testName}"
    mkdir $DIR
    echo "$input" > "${DIR}/input.txt"
    echo $DIR
    echo "$input"
    ../../src/main "$input" >  "${DIR}/output.log"
    echo ""
    i=$((i+1))
    echo $i
}

test single_a "a"
test zero_arity "a()"
test embedded_atoms "a(b, c)"
test multiset_of_2 "a, d"
test multiset_of_4 "d, c, b, a"
test multiset_in_parenthesis "d, ((c, b), a)"
test freelink1 "a(B, c()), d"
test freelink2 "a(B, c(d, E))"
test locallink_zero_indeg "\A.A -> a"
test locallink_1_indeg "\A.\C.(A -> a(B, C), C -> g)"
test a_rule_a2b "a :- b"
test null_rhs "a :- . b"
test null_lhs ":- a. b"
test rule_on_rhs "a :- (b :- c)"
test locallink_on_lhs1 "\X Y.(a, c(d)) :- b, c . d."
test locallink_on_lhs2 "\X.\Y.(X -> a(X), c(d(X))) :- b, c . d."
