* 7-2-2016
Generated the following files:
matmult_7_2_2016_dyn_ops_on_no_letrec_casts.pdf
- matrix multiply 
- dynamic operations turn *on*
- 200 x 200 matrices
- no letrec casts
quicksort_7_2_2016_dyn_ops_on_no_letrec_casts.pdf
- worst case quicksort 
- dynamic operations turn on
- (5000 elements I think but am uncertain)
- no letrec casts
matmult_7_2_2016_dyn_ops_off_no_letrec_casts.pdf
- matrix multiply 
- dynamic operations turn *off*
- 200 x 200 matrices
- no letrec casts
- matrix multiply with dynamic operations turn on and no letrec casts
quicksort_7_2_2016_dyn_ops_off_no_letrec_casts.pdf
- worst case quicksort 
- 5000 elements 
- dynamic operations turn *on*
- no letrec casts

These benchmarks were run on the lab machine shared with chris.
I couldn't run the plots there so they were generated on my computer.
These tests are still kinda invalid because the don't have letrec casts.
In all cases coercions are on the left and twosomes are on the right

Observations:
- matrix multiply got better while quicksort got worse with dynamic operations
- The scale isn't enough to notice anything beyond 20x
- There still isn't any perspective in the tests. Where does racket fall?
- There needs to be titles and stats

TODO:
- enable letrec casts
- add benchmarks for gambit, racket, and cwith closures.
- rerun with larger sample sizes, multiple trials
- extend scale to include 50x
- label coercions and type-based casts
- extend script to automagically do both tests without me having to manually run it twice.


