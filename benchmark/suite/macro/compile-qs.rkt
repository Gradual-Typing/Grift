#lang racket

(require "../../../src/compile.rkt")

(define (cc i o r d)
 (compile i #:output (build-path o)  #:cast-rep r #:dyn-ops d #:mem (expt 1024 3)))

(cc "src/quicksort_worstcase_dyn.grift" "dco" 'Coercions '#t)
(cc "src/quicksort_worstcase_dyn.grift" "dcn" 'Coercions '#f)
(cc "src/static/quicksort_worstcase_static.grift" "sco" 'Coercions '#t)
(cc "src/static/quicksort_worstcase_static.grift" "scn" 'Coercions '#f)
