#lang typed/racket

(require Schml/framework/build-compiler)

(provide (all-defined-out))

(define-type (Result A)  (U error (success A)))
(struct error ([value : Any]))
(struct (A) success ([value : A]))
(define-predicate Any? Any)

(provide (struct-out Config))
(: compiler-config (Parameter Config))
(define compiler-config 
  (make-parameter
   (Config 'Lazy-D)))

;; This is the main compiler it is composed of several micro
;; compilers for successivly smaller langages. 
(: compile/conf (Path Config . -> . (Result Any)))
(define (compile/conf path config)
  (local-require Schml/compiler/schml/reduce-to-cast-calculus
		 Schml/compiler/casts/impose-cast-semantics
		 Schml/compiler/closures/make-closures-explicit
		 )
  (call-with-exception-handler 
   error
   (lambda ()
     (let* ([c0  (reduce-to-cast-calculus path config)]
	    ;;[_  (begin (print c0) (newline))]
	    [l0  (impose-cast-semantics c0 config)]
	    ;;[_  (begin (print l0) (newline))]
	    [uil  (make-closures-explicit l0 config)])
       (success uil)))))

(: compile (Any . -> . (Result Any)))
(define (compile path) 
  (let ((config (compiler-config))) 
    (cond
     [(string? path) (compile/conf (string->path path) config)]
     [(path? path) (compile/conf path config)]
     [else (error 'compile)])))

#|
(module+ main
  (command-line 
     #:program "Schml-compiler-tests"
     #:args (str) (compile str)))
|#
