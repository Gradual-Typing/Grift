#lang typed/racket

(require Schml/framework/build-compiler)
	   
(provide (all-defined-out))

(define-type Result (U error success))
(struct: error ([value : exn]))
(struct: success ())


(define: default-compiler-config : (Parameter Config)
  (make-parameter
   (Config 'Lazy-D)))
    
(define: (compile/conf [path : Path] [config : Config]) : Result 
  (local-require Schml/compiler/read)
  (with-handlers ((exn? (lambda: ((e : exn)) (error e))))
    (let* ((stx (read path config)))
      (success))))

(define: (compile [path : (U Path String)]) : Result
  (let ((config (default-compiler-config))) 
    (if (string? path)
	(compile/conf (string->path path) config)
	(compile/conf path config))))

