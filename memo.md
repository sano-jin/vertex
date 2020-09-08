# Memo

## 記号
- ポインタは`_` でアクセス制御（スコープ外からはアンダースコアを一つ外したように見える）
  - `A -> a, A -> a` => `Error: Not functional`
  - `A -> a, (A -> a(_A, A))` == `A -> a, B -> a(A, B)`
  - `A -> a, (_A -> a)` == `A -> a, A -> a` => `Error: Not functional` 
  - `a(B), (_B -> b(_B))` == `a(B), B -> b(B)` 
  - `a(B), (*B -> b(B))` == `a(B), B -> b(B)`
  - `(A -> a), (A -> a)` == `A -> a, B -> a`
  - `(_A -> a), (_A -> a)` == `A -> a, A -> a` => `Error: Not functional` 
  - `A -> a(B), (_B -> B, (_B -> b))` == `a(b)`
  - `A -> a(B), {_B -> B, {_B -> b}}` == `a -> B, {{__B -> b}}`
- モジュールは`::`で示す
- `>_`, `<_`
 
 
