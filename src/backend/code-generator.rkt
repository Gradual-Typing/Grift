#lang racket/base

(require 
 "../configuration.rkt"
 (prefix-in c: "./c/code-generator.rkt")
 (prefix-in sham: "./sham/code-generator.rkt"))

(provide generate-code)

;; Basic driver for the entire backend
(define (generate-code uil) 
  (case (backend)
    [(C) (c:generate-code uil)]
    [(LLVM) (sham:generate-code uil)]))
