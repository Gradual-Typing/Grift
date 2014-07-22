#lang typed/racket

(require Schml/framework/build-compiler)
	   
(provide (all-defined-out))

(define-type Result (U error success))
(struct error ([value : exn]))
(struct success ())

(define compiler-config : (Parameter Config)
  (make-parameter
   (Config 'Lazy-D)))
    
(define (compile/conf [path : Path] [config : Config]) : Result 
  (local-require Schml/compiler/read)
  (with-handlers ((exn? (lambda: ((e : exn)) (error e))))
    (let* ((stx-prog (read path config))
	   (core-forms (parse stx-prog config)))
      (success))))

(define (compile [path : (U Path String)]) : Result
  (let ((config (compiler-config))) 
    (if (string? path)
	(compile/conf (string->path path) config)
	(compile/conf path config))))

