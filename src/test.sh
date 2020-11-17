#!/bin/bash
test() {
  input="$1"
  echo \""$input"\"
  ./main "$input"
}

test "a"
test "a()"
test "a(b, c)"
test "a(b, c), d"
test "d, c, b, a"
test "d, ((c, b), a)"
test "a(B, c()), d"
test "a(B, c(d, E))"
test "A -> a()"
test "A -> a(B, C), C -> g"
test "a :- . b"
test ":- a. b"
test "a :- (b :- c)"
test "\X.\Y.(a, c(d)) :- b, c . d."
test "\X.\Y.(X -> a(X), c(d(X))) :- b, c . d."
