# Directed HyperLMNtal

![Image of DHLMNtalIcon](https://github.com/sano-jin/vertex/blob/master/doc/DHLMNtal-logo-red.png)


Directed HyperLMNtal (DHLMNtal) is a derived calculus model of LMNtal.

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

Here is a syntax. Notice `P` is a process (= program).

```
P (process) ::= ()                          (null)
            | X -> p(X1, ..., Xm)  (m >= 0) (atom)
            | (P, P)                        (molecule)
            | \X.P                          (link creation)
            | (P :- P)                      (rule)
```

Basicaly, the reduction (calculation step) proceeds as `(P, (P :- Q)) --> (Q, (P :- Q))`.

That is, the rule will rewrite the process that matches to its left hand-side to its right hand-side.

See [here](https://github.com/sano-jin/vertex/blob/master/semantics.md) to get more description about semantics.

Also, take a look at the [Slide](https://github.com/sano-jin/vertex/blob/master/doc/DHLMNtal.pdf) (its written in Japanese though).

## Usage

### Requirement
- stack (haskell)

### How to use

```bash
git clone https://github.com/sano-jin/vertex.git
cd vertex
stack run example/sample.dhl  # run the program "sample.dhl"
```

If the `sample.dhl` was the following,
```
a(b). a(X) :- b. b :- c.
```

the result should be like the following.
```
0: 
L1 -> b. a(L1). 
a(X) :- b. b :- c. 

1: a(X) :- b ~> 
b. b. 
a(X) :- b. b :- c. 

2: b :- c ~> 
c. b. 
a(X) :- b. b :- c. 

3: b :- c ~> 
c. c. 
a(X) :- b. b :- c. 
```

### Non-deterministic execution
Experimental.

Put `--nd` for the "non-deterministic" execution.
```bash
stack run -- example/sample_nd.dhl --nd
```

### Visualizer
Experimental.

Currently able to show only the initial state.

```bash
stack run dhlvis -- example/sample_list.dhl
```

![Image of the Visualizer](https://github.com/sano-jin/vertex/blob/master/doc/dhlvis_image.png)


## Development

### Test
Run this test to obtain more detailed information about the state of the heap, etc.

```shell
stack test --test-arguments "example/sample.dhl"
```




