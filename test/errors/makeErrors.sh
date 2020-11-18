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

test "a X"
test "a a a X -> a"
test "a , (X)"
test "a(B, c(d, E))"
test "A -> a()"
test "A -> a(B, C), C -> g"
test "A -> a :- b"
test "(a :- b) :- c"
test "A -> a, g :- b. f"
test "\X.\Y.(a(X), c(d(X))) :- b, c . d."
