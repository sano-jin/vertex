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
    stack exec dhli -- "${DIR}/input.dhl" --nd >  "${DIR}/output.log"
    echo ""
    i=$((i+1))
    echo $i
}

test single_a                "a, a, a. a :- ."
test append                  \
"append(cons(a, cons(b, nil)), cons(c, nil)).
R -> append(cons(H, T), L) :- R -> cons(H, append(T, L)).
R -> append(nil, L) :- R -> nil."
test abd                    "a. a :- b. b :- c."
test ring                   "a. a:-b.b:-c.c:-d.d:-e.e:-f.f:-a."
test nd3                    \
"a.
a :- b.
a :- c.
a :- b, c.
b :- c.
c :- d, d.
d, d, d, d :- d, d."
test nd4                   \
"a, a.
a:-b.
a:-c.
a:-d.
d,d :- a,a."
test honey                 \
"r, r, r, r, l, l, l, l, b.
b, r :- s.
b, l :- s.
r, s :- r, b.
l, s :- l, b"
test triangle             \
" \X Y Z.(
   a(X, Y), a(Y, Z), a(Z, X),
   X -> link, Y -> link, Z -> link
 ).
 
 \X Y Z.(
   a(X, Y), a(Y, Z), a(Z, X),
   X -> link, Y -> link, Z -> link
 )
 :-
 \X Y Z.(
   a(X, Y), a(X, Z), a(Z, Y),
   X -> link, Y -> link, Z -> link
 )."

test incr3              \
"

\X.(
    proc(load, X, 0),
    proc(load, X, 0),
    proc(load, X, 0),
    X -> 0
).



% Load the global variable

proc(load, X, 0), X -> 0
:- proc(incr, X, 0), X -> 0.

proc(load, X, 1), X -> 0
:- proc(incr, X, 0), X -> 0.

proc(load, X, 2), X -> 0
:- proc(incr, X, 0), X -> 0.

proc(load, X, 3), X -> 0
:- proc(incr, X, 0), X -> 0.


proc(load, X, 0), X -> 1
:- proc(incr, X, 1), X -> 1.

proc(load, X, 1), X -> 1
:- proc(incr, X, 1), X -> 1.

proc(load, X, 2), X -> 1
:- proc(incr, X, 1), X -> 1.

proc(load, X, 3), X -> 1
:- proc(incr, X, 1), X -> 1.


proc(load, X, 0), X -> 2
:- proc(incr, X, 2), X -> 2.

proc(load, X, 1), X -> 2
:- proc(incr, X, 2), X -> 2.

proc(load, X, 2), X -> 2
:- proc(incr, X, 2), X -> 2.

proc(load, X, 3), X -> 2
:- proc(incr, X, 2), X -> 2.


proc(load, X, 0), X -> 3
:- proc(incr, X, 3), X -> 3.

proc(load, X, 1), X -> 3
:- proc(incr, X, 3), X -> 3.

proc(load, X, 2), X -> 3
:- proc(incr, X, 3), X -> 3.

proc(load, X, 3), X -> 3
:- proc(incr, X, 3), X -> 3.



% Increment the local variable

proc(incr, X, 0)
:- proc(store, X, 1).

proc(incr, X, 1) 
:- proc(store, X, 2).

proc(incr, X, 2) 
:- proc(store, X, 3).

proc(incr, X, 3) 
:- proc(store, X, 4).



% Store the global variable

proc(store, X, 0), X -> 0
:- proc(halt, X, 0), X -> 0.

proc(store, X, 1), X -> 0
:- proc(halt, X, 1), X -> 1.

proc(store, X, 2), X -> 0
:- proc(halt, X, 2), X -> 2.

proc(store, X, 3), X -> 0
:- proc(halt, X, 3), X -> 3.


proc(store, X, 0), X -> 1
:- proc(halt, X, 0), X -> 0.

proc(store, X, 1), X -> 1
:- proc(halt, X, 1), X -> 1.

proc(store, X, 2), X -> 1
:- proc(halt, X, 2), X -> 2.

proc(store, X, 3), X -> 1
:- proc(halt, X, 3), X -> 3.


proc(store, X, 0), X -> 2
:- proc(halt, X, 0), X -> 0.

proc(store, X, 1), X -> 2
:- proc(halt, X, 1), X -> 1.

proc(store, X, 2), X -> 2
:- proc(halt, X, 2), X -> 2.

proc(store, X, 3), X -> 2
:- proc(halt, X, 3), X -> 3.


proc(store, X, 0), X -> 3
:- proc(halt, X, 0), X -> 0.

proc(store, X, 1), X -> 3
:- proc(halt, X, 1), X -> 1.

proc(store, X, 2), X -> 3
:- proc(halt, X, 2), X -> 2.

proc(store, X, 3), X -> 3
:- proc(halt, X, 3), X -> 3."

test incr3WithGuard        \
"
\X.(
    proc(load, X, 0),
    proc(load, X, 0),
    proc(load, X, 0),
    X -> 0
).

% Load the global variable
proc(load, X, \$y:int), X -> \$x:int
:- proc(incr, \$x, 0), X -> \$x.

% Increment the local variable
proc(incr, X, \$y)
:- \$y' := \$y + 1 
|  proc(store, X, \$y').

% Store the global variable
proc(store, X, $y:int), X -> $x:int
:- proc(halt, X, $y:int), X -> $y:int.
"
