#lang typed/racket

(provide (struct-out File)
	 Stx)

;; This is the type returned by the racket reader
(define-type Stx (Syntaxof Any))

(struct: File 
	 ([name : String]
	  [syntax* : (Listof Stx)]))
