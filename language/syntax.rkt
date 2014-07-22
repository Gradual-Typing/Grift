#lang typed/racket

(provide (all-defined-out))

;; This is the type returned by the racket reader

(define-type Stx (Syntaxof Any))

(struct: Stx-Prog
	 ([name : String]
	  [syntax* : (Listof Stx)]))
