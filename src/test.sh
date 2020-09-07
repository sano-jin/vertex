#!/bin/bash
parse() {
  input="$1"
  echo \""$input"\"
  ./parser "$input"
}

parse "a"
parse "a()"
parse "a(b, c)"
parse "a(b, c), d"
parse "a(B, c()), d"
parse "a(B, c(d, E))"
parse "A -> a()"
parse "*A -> a()"
parse "A -> a(B, C), *C -> g"
parse "A -> a() :- b"
parse "A -> a :- (b :- c)"
parse "A -> a, g :- b. f"
parse "A -> a() :- b. f."
parse "a :- b, c . d ."
