This is a relatively bare-bones version of miniKanren implemented in Typed Racket.  The only constraint currently supported is unification (`==`).  We use a fair/interleaved search strategy, consistent with the spirit of miniKanren.
To build goals, we support:
- `(fresh (x ...) goal0 goal ...)`
- `(conj goal0 goal ...)`
- `(disj goal0 goal...)`
- `(conde ([goal0 goal ...] ...))`
- `(ifte g0 g1 g2)`
- `(once g)`
- `fail`
- `succeed`
- `(== term0 term1)`

where terms are given by the type:
```
(define-type Term 
  (U var
     Number
     Symbol
     Null 
     (Pair Term Term)))
```
To obtain a list of solutions, use:
- `(run N x goal0 goal ...)`
- `(run N (x ...) goal0 goal ...)`
- `(run* x goal0 goal ...)` 
- `(run* (x ...) goal0 goal ...)`
where `N` is a non-negative integer bounding the number of desired solutions.  If you use `N = #f`, that's equivalent to using `run*`.

This implementation can only be used by other _Typed_ Racket modules due to 
[restrictions](https://docs.racket-lang.org/ts-guide/caveats.html#%28part._.Macros_and_compile-time_computation%29)
on how macros exported from typed modules can be used.

On the plus side though, Typed Racket clients do not incur any contract checking overhead that would happen if they instead imported an untyped miniKanren implementation.

We implement backtracking search using an adaptation of the dual-continuation 
backtracking monad described by Kiselyov et al. in their ICFP 2005 paper 
[Backtracking, Interleaving, and Terminating Monad Transformers](http://homes.sice.indiana.edu/ccshan/logicprog/LogicT-icfp2005.pdf).

Substitutions are represented using a skew binary random access list, as described in 
[Efficient Representations for Triangular Substitutions](https://www.researchgate.net/publication/238089215_Efficient_representations_for_triangular_substitutions_A_comparison_in_miniKanren).
