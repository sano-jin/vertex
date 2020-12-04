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


test not_parened_link "a X"
test no_comma_separated_atom "a a a X -> a"
test no_atom_name "a , (X)"
test freelink_on_top_level "a(B, c(d, E))"
test freelink_on_top_level "A -> a()"
test freelink_on_top_level "a(B, c()), d"
test freelink_on_top_level "a(B, c(d, E))"
test freelink_on_top_level "A -> a(B, C), C -> g"
test not_redirected "A -> a :- b"
test rule_on_lhs "(a :- b) :- c"
test not_redirected "A -> a, g :- b. f"
test not_serial_in_rule "\X.\Y.(a(X), c(d(X))) :- b, c . d."
