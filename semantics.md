# Formal semantics of Directed HyperFlatLMNtal 

## Syntax

### Links and Atoms

DHLMNtal has two kinds of identifiers.

- Link name
  - `X` denotes a link name.
  - In the concrete syntax, link names are denoted by identifiers starting from capital letters.
- Atom name
  - `p` denotes an atom name.
  - In the concrete syntax, atom names are denoted by identifiers starting from small lettes (for now).

### Processes

```
P (process) ::= ()                           (null)
             | X -> p(X1, ..., Xm)  (m >= 0) (atom)
             | (P, P)                        (molecule)
             | \X.P                          (link creation)
             | (P :- P)                      (rule)
```
Fig.1. Syntax

Given a link `X`, we define its _head_ as an atom `X -> p(X1, ..., Xm)`.
Also, the _tail_ of the link `Xi` as the i-th port of the atom.
Here we shall also call `X` as the _incoming link_ of the atom and `Xi` as the _outgoing link_ of the atom.
An unary atom `X -> Y` is called an _indirection_ atom, which can be read as aliasing `X` to `Y`.

The set of the free link names in a process `P` is denoted as `fn(P)` and is defined inductively as Fig.2.

```
fn(())                  = {}
fn(X -> p(X1, ..., Xm)) = {X, X1, ..., Xm}
fn((P, Q))              = fn(P) `union` fn(Q)
fn(\X.P)                = fn(P) \ {X}
fn((P :- Q))            = {}
```
Fig.2. Free link names of a process

We assume that the set of free link names of the process which consists the whole program, the top level process, is an empty set. This can be easily achieved by just adding some extra link creations to the top level process if needed.

There are several conditions processes must satisfy.

#### No circular aliasing condition :
When applying the latter 2 conditions,
aliasing should not form a cycle.

That is, when traversing aliasing, there should be no same link name appears more than once. 

#### Functional (right-unique) condition :
For all link `X` in `fn(P)`, 
the _head_ of `X` must not appear more than once in `P`. 

#### Serial (left-total) Condition:
Given an process `\X.P` where `X` in `fn(P)`, the _head_ of the `X` must exist in `P`.

### Rules
Given a rule `(P :- Q)`, `P` is called the left-hand side and `Q` is called the right-hand side of the rule.

There are several conditions a rule `(P :- Q)` must satisfy the following conditions.

1. `fn(P)` should be a super set of `fn(Q)`.
1. Rule must not appear in `P`
1. If the _head_ of a link `X` occurs in `P`, the _head_ of the link `X` must also occur in `Q`.

## Operational Semantics

We first define structural congruence `==` and then define the reduction relation `~>` on proceses.

### Structural congruence

We define the relation `==` on processes as the minimal equivalence relation satisfying the rules shown in Fig.3.

Where `P[Y / X]` is a link substitution that replaces all free occurrences of `X` with `Y`.
If a free occurrence of `X` occurs in a location where `Y` would not be free, alpha-conversion (E1) may be required.

1. `((), P) == P`
1. `(P, Q) == (Q, P)`      
1. `(P, (Q, R)) == ((P, Q), R)`      
1. `P == P'  =>  (P, Q) == (P', Q)`
1. `\X.P == \Y.P[Y / X]`      
   where `Y` is not in `fn(P)`
1. `\X.(X -> Y, P) == P[Y / X]`
1. `\X.() == ()`
1. `\X.\Y.P == \Y.\X.P`
1. `\X.(P,Q) == (\X.P,Q)`
   where `X` is not in `fn(Q)`

Fig.3. Structural congruence on processes}

### Reduction relation

We define the reduction relation `~>` on processes as the minimal relation satisfying the rules in Fig.4.

1. `P ~> P'  =>  (P, Q) ~>  (P', Q)`
1. `P ~> P'  =>  \X.P ~>  \X.P'`            
1. `Q == P` /\ P ~> P' /\ P' == Q'  =>  Q ~> Q'`
1. `(P, (P :- Q)) ~> (Q, (P :- Q))`

Fig.4. Reduction relation on processes




