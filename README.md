# Directed HyperFlatLMNtal

## Syntax
DHLMNtal has two kinds of identifiers.

- Link name
  - `X` denotes a link name.
  - In the concrete syntax, link names are denoted by identifiers starting from capital letters.
- Atom name
  - `p` denotes an atom name.
  - In the concrete syntax, atom names are denoted by identifiers starting from small lettes (for now).

Given a link X, we define its head as an atom `X -> p(X1, ..., Xm)`.
Also, the tail of the link `Xi` as the i-th port of the atom.

Here we shall also call `X` as the incoming link of the atom and `Xi` as the outgoing link of the atom.

An unary atom `X -> Y` is called a aliasing, which can be read as aliasing `X` to `Y`.

```
P ::= 0                  --          (null)
  | X -> p(X1, ..., Xm)  -- (m >= 0) (atom)
  | (P, P)               --          (molecule)
  | \X.P                 --          (link creation)
  | (P :- P)             --          (rule)
```
