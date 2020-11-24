#!/bin/bash
test() {
  input="$1"
  echo \""$input"\"
  ./main "$input"
  echo ""
}

test "X -> a(Y, Z)"
test "X -> a(Y, X)"
test "X -> a(X, Y)"
test "X -> a(c, X)"
test "\X.(X -> a(Y, X)), g(h)"
test "X -> a(Y, X), Y -> c"
