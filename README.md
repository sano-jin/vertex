# Directed HyperFlatLMNtal

Directed HyperFlatLMNtal (DHLMNtal) is a derived calculus model of LMNtal.

- Focused on concurrency
- Can easily deal with hypergraphs representing structures and pointers

## Definition
DHLMNtal has two kinds of identifiers.

- Link name
  - `X` denotes a link name.
  - In the concrete syntax, link names are denoted by identifiers starting from capital letters.
- Atom name
  - `p` denotes an atom name.
  - In the concrete syntax, atom names are denoted by identifiers starting from small lettes (for now).

```
P ::= 0                           (null)
  | X -> p(X1, ..., Xm)  (m >= 0) (atom)
  | (P, P)                        (molecule)
  | \X.P                          (link creation)
  | (P :- P)                      (rule)
```
Fig.1. Syntax

Given a link `X`, we define its head as an atom `X -> p(X1, ..., Xm)`.
Also, the tail of the link `Xi` as the i-th port of the atom.
Here we shall also call `X` as the incoming link of the atom and `Xi` as the outgoing link of the atom.
An unary atom `X -> Y` is called an aliasing atom, which can be read as aliasing `X` to `Y`.

The set of the free link names in a process `P` is denoted as `fn(P)` and is defined inductively as Fig.2.

```
fn(0)                   = {}
fn(X -> p(X1, ..., Xm)) = {X, X1, ..., Xm}
fn((P, Q))              = fn(P) `union` fn(Q)
fn(\X.P)                = fn(P) \ {X}
fn((P :- Q))            = {}
```
Fig.2. Free names of a process




