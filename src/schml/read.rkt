#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/read                                                  |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription: This pass simply reads in a file given a path. The reader is 
|the function read-syntax so that the source locations may be lifted from the
|syntax-objects as we convert them to core forms.
+------------------------------------------------------------------------------|#
(require schml/src/language
         schml/src/errors)
(provide read)

#|
Function: get-name-of-file
This helper function called by read extracts a file name from a path.
|#
(define (get-name-of-file [p : Path]) : String
  (let ([maybe-name (file-name-from-path p)])
    (if maybe-name
	(path->string maybe-name)
	(raise-file-name-exn p))))

#|
Function: get-reader
This helper function called by read-syntax-from-file creates a syntax-reader
that annotates the syntax with source location and file name.
|#
(define (get-reader [name : String]) : (Input-Port -> (U Stx EOF))
  (lambda ([p : Input-Port]) : (U Stx EOF)
     (cast (read-syntax name p) (U Stx EOF))))

#|
Function: read-syntax-from-file
This helper function called by read collects all syntax in a file and returns
it as a list.
|#
(: read-syntax-from-file (Path String . -> . (Listof Stx)))
(define (read-syntax-from-file [path : Path] [name : String]) : (Listof Stx)
  (call-with-input-file path
    (lambda ([p : Input-Port])
      (let ((read (get-reader name)))
	(letrec ([loop : ((U Stx EOF) -> (Listof Stx)) 
		  (lambda ([stx : (U Stx EOF)])
		    (if (eof-object? stx)
			'()
			(cons stx (loop (read p)))))])
	  (loop (read p)))))
    #:mode 'text))


#|
Pass: read
Collects the syntax from a file and returns it a Stx-Prog ast.
|#
(: read (Path Config . -> . Syntax-Lang))
(define (read path config)
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
    (let ([name : String (get-name-of-file path)])
      (Prog name (read-syntax-from-file path name)))))



