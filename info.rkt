#lang info
(define collection "grift")
(define version "0.1")
(define deps
  '(;; Since sham isn't on the package server yet we use a direct link to github
    "https://github.com/rjnw/sham.git/?path=sham-llvm#3dd8cacb82bb4c77b447f76d984fee4f2aae9f0a"
    "https://github.com/rjnw/sham.git/?path=sham-lib#3dd8cacb82bb4c77b447f76d984fee4f2aae9f0a"
    "https://github.com/rjnw/scf.git/?path=scf-lib#a9405041f636ac7c4368b736d10f3f8a858eed74"))
(define racket-launcher-names '("grift" "grift-bench" "grift-configs"))
(define racket-launcher-libraries '("main.rkt" "benchmark/bench.rkt" "benchmark/configs.rkt"))
(define post-install-collection "src/backend-c/runtime/make.rkt")
(define compile-omit-files
  '("src/backend-c/runtime"))
(define clean
  '("src/backend-c/runtime/boehm-gc-install"
    "src/backend-c/runtime/cast-profiler.o"
    "src/backend-c/runtime/nonegc.o"
    "src/backend-c/runtime/hashcons.o"
    "src/backend-c/runtime/cast_queue.o"
    "src/backend-c/runtime/suspended_cast.o"
    "src/backend-c/runtime/runtime.o"))

(define test-include-paths
  (list #rx".rkt$"))

