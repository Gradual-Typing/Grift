#lang typed/racket

(require Schml/language/file-syntax
         Schml/framework/build-compiler)

(provide read)

;; The read pass takes a file-path and returns a s-expression

(define: (read [path : Path] [config : Config]): File
  (parameterize
      ;; The following parameters change what the reader is willing
      ;; to accept as input.
      ([read-case-sensitive #t]
       [read-square-bracket-as-paren #t]
       [read-curly-brace-as-paren #t]
       [read-accept-box #f]
       [read-accept-compiled #f]
       [read-accept-bar-quote #f]
       [read-accept-graph #f]
       [read-decimal-as-inexact #f]
       [read-accept-dot #f]
       [read-accept-infix-dot #f]
       [read-accept-quasiquote #f]
       [read-accept-reader #f]
       [read-accept-lang #f])
    (let ([name (get-name-of-file path)])
      (File name (read-syntax-from-file path name)))))

(define: (get-name-of-file [path : Path]) : String
  (let ((maybe-name (file-name-from-path path)))
    (if maybe-name
	(path->string maybe-name)
	(raise 'file-name))))

(define: (get-reader [name : String]) : (Input-Port -> (U Stx EOF))
  (lambda: ([p : Input-Port]) 
    (read-syntax name p)))

(define: (read-syntax-from-file [path : Path] [name : String]) : (Listof Stx)
  (call-with-input-file path
    (lambda: ([p : Input-Port])
      (let ((read (get-reader name)))
	(letrec: ([loop : ((U Stx EOF) -> (Listof Stx)) 
		  (lambda: ([stx : (U Stx EOF)])
		    (if (eof-object? stx)
			'()
			(cons stx (loop (read p)))))])
	  (loop (read p)))))
    #:mode 'text))


