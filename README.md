This is a relatively bare-bones version of miniKanren implemented in Typed Racket.  The only constraint currently supported is unification (`==`).  To build goals, we support 
- `(fresh (x ...) goal0 goal ...)`
- `(conj goal0 goal ...)`
- `(disj goal0 goal...)`
- `(conde [(goal0 goal ...) ...])`
- `(ifte g0 g1 g2)`
- `(once g)`
- `fail`
- `succeed`
- `(== term0 term1)`

where
```
(define-type Term 
  (U var
     Number
     Symbol
     Null 
     (Pair Term Term)))
```

This implementation can only be imported by other _Typed_ Racket modules due to 
[restrictions](https://docs.racket-lang.org/ts-guide/caveats.html#%28part._.Macros_and_compile-time_computation%29)
on how macros exported from typed modules can be used.

On the plus side though, Typed Racket clients do not incur any contract checking overhead that would happen if they instead imported an untyped miniKanren implementation.

We implement backtracking search using an adaptation of the dual-continuation 
backtracking monad described by Kiselyov et al. in their ICFP 2005 paper 
[Backtracking, Interleaving, and Terminating Monad Transformers](http://homes.sice.indiana.edu/ccshan/logicprog/LogicT-icfp2005.pdf).

Substitutions are represented using a skew binary random access list, as described in 
[Efficient Representations for Triangular Substitutions](https://www.researchgate.net/publication/238089215_Efficient_representations_for_triangular_substitutions_A_comparison_in_miniKanren).
