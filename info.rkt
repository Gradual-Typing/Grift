#lang info
(define collection "grift")
(define version "0.1")
(define deps
  '(;; Since sham isn't on the package server yet we use a direct link to github
    "https://github.com/rjnw/sham.git/?path=sham-llvm#45f959e6893dceff9a4eb5313dfccbe9d287428c"
    ;; FIXME this is pointing to my pull request that fixes a bug in sham
    "https://github.com/akuhlens/sham/?path=sham-lib#432272371baf91fa45e6d5807e49127b488b33f3"
    "https://github.com/rjnw/scf.git/?path=scf-lib#a9405041f636ac7c4368b736d10f3f8a858eed74"))
(define racket-launcher-names '("grift" "grift-bench" "grift-configs"))
(define racket-launcher-libraries '("main.rkt" "benchmark/bench.rkt" "benchmark/configs.rkt"))
(define post-install-collection "src/backend/runtime/make.rkt")
(define compile-omit-files '("src/backend/runtime"))
(define clean
  '("src/backend/runtime/boehm-gc-install"
    "src/backend/runtime/cast-profiler.o"
    "src/backend/runtime/nonegc.o"
    "src/backend/runtime/hashcons.o"
    "src/backend/runtime/cast_queue.o"
    "src/backend/runtime/suspended_cast.o"
    "src/backend/runtime/runtime.o"))

(define test-include-paths
  (list #rx".rkt$"))

