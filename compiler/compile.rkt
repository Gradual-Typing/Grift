#lang typed/racket

(require Schml/framework/build-compiler)

(provide (all-defined-out))

(define-type Result (U error success))
(struct error ([value : Any]))
(struct success ())
(define-predicate Any? Any)

(provide (struct-out Config))
(define compiler-config : (Parameter Config)
  (make-parameter
   (Config 'Lazy-D)))

;; This is the main compiler it is composed of several micro
;; compilers for successivly smaller langages. 
(: compile/conf (Path Config . -> . Result))
(define (compile/conf path config)
  (local-require Schml/compiler/schml/reduce-to-cast-calculus
		 Schml/compiler/casts/interpret-casts
		 Schml/compiler/closures/make-closures-explicit)
  (call-with-exception-handler 
   error
   (lambda ()
     (let* ([prgm/casts  (path->cast-calculus path config)]
	    [prgm/lambda (interpret-casts prgm/casts config)]
	    [prgm/data   (make-closures-explicit prgm/lambda config)])
  
       (success)))))

(: compile ((U Path String) . -> . Result))
(define (compile path) 
  (let ((config (compiler-config))) 
    (if (string? path)
	(compile/conf (string->path path) config)
	(compile/conf path config))))


