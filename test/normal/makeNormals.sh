#!/bin/bash
i=0

test() {
    input="$1"
    DIR="test${i}"
    mkdir $DIR
    echo "$input" > "${DIR}/input.txt"
    echo $DIR
    echo "$input"
    ../../src/main "$input" >  "${DIR}/output.log"
    echo ""
    i=$((i+1))
    echo $i
}

test "a"
test "a()"
test "a(b, c)"
test "a(b, c), d"
test "d, c, b, a"
test "d, ((c, b), a)"
test "a(B, c()), d"
test "a(B, c(d, E))"
test "\A.A -> a()"
test "\A.\C.(A -> a(B, C), C -> g)"
test "a :- . b"
test ":- a. b"
test "a :- (b :- c)"
test "\X.\Y.(a, c(d)) :- b, c . d."
test "\X.\Y.(X -> a(X), c(d(X))) :- b, c . d."
