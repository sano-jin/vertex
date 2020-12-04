# Directed HyperLMNtal

![Cube](https://github.com/sano-jin/vertex/blob/develop/doc/dhlvis_cube.png)

*This is an experimental project*

Directed HyperLMNtal (DHLMNtal) is a derived calculus model of LMNtal.
It is designed to be able to deal with hypergraphs representing structures and pointers easily
without having dangling pointers.

## Installization
- Requirement
  - stack (haskell).

```bash
git clone https://github.com/sano-jin/vertex.git
cd vertex
stack build  
```

## Introduction

### Atoms and Links
DHLMNtal program is consists of graphs. Graphs are consists of Atoms (= nodes) and Links (= directed hyper edges).

- Links
  - links are denoted by identifiers starting from capital letters.
  - When representing a graph, links must appear inside of the *link creation* `\X1 X2 ... Xn.<inside the link creation>`.
  - E.g. `X`, `Y`, ...
- Atoms
  - An atom has an *incomming link*, its name and 0 or more *outgoing links*.
    Denote them as `<The incomming link> "->" <the name of the atom> "(" <the outgoing links> ")"`.
    - The name can be an identifier starting from small lettes.
    - You can omit the parentheses if there is no outging link and
    - you can omit the incomming link if the link doesn't appear other than it.
  - E.g. `X -> a(Y, Z)`, `X -> b`, `c`, ...

Separate atoms with commas.

For example, you may write like this.
```
\X.(a(X), X -> b)
```

Open your favorite editor, write down the former program, save it as `test1.dhl` and visualize it with the `dhlvis`.

```bash
stack exec dhlvis -- test1.dhl
```

![The graphical image of the test1.dhl](https://github.com/sano-jin/vertex/blob/develop/doc/dhlvis_sample1-1.png)

Press ESC to terminate.

### Rules

Besides the graphs, you can write the (rewriting) *rule* `<left hand side:subgraph to match> :- <right hand-side: subgraph to yields>`.
The rule will rewrite the graph to its right hand-side if it's left hand-side mathces.
Notice the rule remains and tries to rewrite graphs until it fails.

You can use period to separate graphs and rules.

Rewrite the former program as the following
```
\X(a(X), X -> b).
X -> b :- X -> c.
```

and visualize it.

```bash
stack exec dhlvis -- test1.dhl
```

![The graphical image of the test1.dhl](https://github.com/sano-jin/vertex/blob/develop/doc/dhlvis_sample1-1.png)

Press SPACE to reduce (rewrite) 1 step.

If you want the reduction steps in a text, run `dhli`.
```stack 
stack exec dhlvis -- test1.dhl
```

The output will be the following.
```
0: 
a(b). 

1: X -> b :- X -> c ~> 
a(c). 
```
This shows
- The initial state is the `a(b)` (and the rule),
- the reduction `~>` proceeds with a rule `X -> a :- X -> c` and
- the second and the terminal state is `a(c)` (and the rule).

Notice that the rule is hided for the simplicity but it does exist throughout the calculation process.

### Non-detereminisity
DHLMNtal has non-detereminisity.
Rules can rewrite graph as long as they matches, they are not ordered. 

For example, the resulting graph of `a. a :- b. a :- c` can be `b` or `c`.

You can use `--nd` (non-deterministic) option to construct the state space (all possible states and reduction).

This can be applied to the not-terminating program.

For example, `example/nd1.dhl` is a following program.
```
a.

a :- b.
b :- c.
c :- d.
d :- e.
e :- f.
f :- a.
```

Run this as the following.
```bash
stack exec dhlvis  -- example/nd1.dhl --nd
```

Also, you can visualize the state space.

Run `stateViewer` as below.

```bash
stack exec stateViewer  -- example/nd1.dhl --nd
```

![stateViewer_nd1](https://github.com/sano-jin/vertex/blob/develop/doc/stateViewer_nd1.png)

You can easily know that the transitions are forming a cycle and there is no terminal state.

### For more informations
See [here](https://github.com/sano-jin/vertex/blob/master/semantics.md) to get more description about semantics.
[This pdf](https://github.com/sano-jin/vertex/blob/master/doc/Directed_HyperFlatLMNtal.pdf) also describes the semantics.

Also, feel free to take a look at the [Slide](https://github.com/sano-jin/vertex/blob/master/doc/DHLMNtal.pdf) (its written in Japanese though).

## Development

### Test
Run this test to obtain more detailed information about the state of the heap, etc.

```shell
stack test --test-arguments "example/sample.dhl"
```




