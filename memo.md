# Memo

## Syntax
- `X = _*[A-Z][a-zA-Z_]*`
- `p = [a-z][a-zA-Z_]*`

```math
P = X \mapsto -> p(X_1, ..., X_n)
  | P, P
  | P :- P
```
which corresponds to

```
T = x
  | T T
  | \x.T
```


## 記号
- モジュールは`::`で示す
- `>_`, `<_`

### ポインタのアクセス制御
- ポインタは`_` でアクセス制御（スコープ外からはアンダースコアを一つ外したように見える）
- e.g. De Bruisin Index
- 例
  - `A -> a, A -> a` => `Error: Not functional`
  - `A -> a, (A -> a(_A, A))` == `A -> a, B -> a(A, B)`
  - `A -> a, (_A -> a)` == `A -> a, A -> a` => `Error: Not functional` 
  - `a(B), (_B -> b(_B))` == `a(B), B -> b(B)` 
  - `a(B), (*B -> b(B))` == `a(B), B -> b(B)`
  - `(A -> a), (A -> a)` == `A -> a, B -> a`
  - `(_A -> a), (_A -> a)` == `A -> a, A -> a` => `Error: Not functional` 
  - `A -> a(B), (_B -> B, (_B -> b))` == `a(b)`
  - `A -> a(B), {_B -> B, {_B -> b}}` == `a -> B, {{__B -> b}}`

 
 
