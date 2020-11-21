#!/bin/bash
test() {
  input="$1"
  echo \""$input"\"
  ./main "$input"
  echo ""
}

test "a X"
test "a a a X -> a"
test "a , (X)"
test "a(B, c(d, E))"
test "A -> a(), A -> b"
test "\C.A -> a(B, C)"
test "A -> a(B, C), C -> g"
test "A -> a :- b"
test "(a :- b) :- c"
test "A -> a, g :- b. f"
test "\X.\Y.(a(X), c(d(X))) :- b, c . d."
