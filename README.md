This is an implementation of miniKanren in Typed Racket. 

The internal representation is an adaptation of the dual-continuation 
backtracking monad described by Kiselyov et al. in their ICFP 2005 paper [Backtracking, Interleaving, and Terminating Monad Transformers](http://homes.sice.indiana.edu/ccshan/logicprog/LogicT-icfp2005.pdf).

We represent the substitution using a skew binary random access list, as described in [Efficient Representations for Triangular Substitutions](https://www.researchgate.net/publication/238089215_Efficient_representations_for_triangular_substitutions_A_comparison_in_miniKanren).
