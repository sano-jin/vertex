# Directed HyperFlatLMNtal

Directed HyperFlatLMNtal (DHLMNtal) is a derived calculus model of LMNtal.

- Focused on concurrency
- Can easily deal with hypergraphs representing structures and pointers

## Language basis
DHLMNtal has two kinds of identifiers.

- Link name
  - `X` denotes a link name.
  - In the concrete syntax, link names are denoted by identifiers starting from capital letters.
- Atom name
  - `p` denotes an atom name.
  - In the concrete syntax, atom names are denoted by identifiers starting from small lettes (for now).

P is a process (= program).

```
P ::= ()                          (null)
  | X -> p(X1, ..., Xm)  (m >= 0) (atom)
  | (P, P)                        (molecule)
  | \X.P                          (link creation)
  | (P :- P)                      (rule)
```

See [here](https://github.com/sano-jin/vertex/blob/master/semantics.md) to get more description about semantics.

## Usage

### Requirement
- Haskell ghc 

### How to use

```bash
cd src
ghc Main.hs         # compile the implementation
./Main "a. a :- b"  # run program
```

## Development

### Test
See [here](https://github.com/sano-jin/vertex/blob/master/test/README.md) to get description about testing this.




