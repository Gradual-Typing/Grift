#lang typed/racket



(require/typed 
 Schml/framework/build-compiler
 [#:struct compiler-config 
	   ([semantics : Symbol]
	    [trace-passes : (listof Symbol)])])
	   
(provide (all-defined-out))

(define-type Result (U error success))
(struct: error ([obj : exn?]))
(struct: success ())

(: default-compiler-config (Parameter compiler-config?))
(define default-compiler-config
  (make-parameter
   (compiler-config 'lazy-d 'none 'none)))
    


(: compile (Path compiler-config? -> Result))
(define (compile/conf file-name settings) 
  (with-handlers ((exn? (lambda (e) (error e))))
    (let* ()
      (success))))


;; Invokes the compiler on an s-expr obtained by various means
(: compile ((U Path String) -> Result))
(define (compile file)
  (let ((config (default-compiler-config))) 
    (if (string? file)
	(compile/conf (string->path file) config)
	(compile/conf file config))))

