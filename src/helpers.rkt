#lang typed/racket

(provide (all-defined-out))

#| Environments are persistent hash tables |#

#| Source locations always return a string even if it is empty |# 
(require/typed racket/base
	       [(srcloc->string src->str) 
		(srcloc . -> . (U False String))])
(: srcloc->string (srcloc . -> . String))
(define (srcloc->string x) (or (src->str x) ""))

(: syntax->srcloc ((Syntaxof Any) . -> . srcloc))
(define (syntax->srcloc x)
  (srcloc (syntax-source x) 
	  (syntax-line x)
          (syntax-column x) 
	  (syntax-position x) 
	  (syntax-span x)))

(: file->srcloc (String . -> . srcloc))
(define (file->srcloc n)
  (srcloc n #f #f #f #f))

#| 
A language form ends up just being a polymorphic record.
This allows me to make very desciptive grammars via types later on.
|#

(define-syntax (define-forms stx)
  (syntax-case stx ()
    [(_ (name fields ...) f* ...)
     (with-syntax ([(types ...) (generate-temporaries #'(fields ...))])
       #'(begin 
	   (struct (types ...) name ([fields : types] ...) #:transparent)
	   (define-forms f* ...)))]
    [(_) #'(void)]))

(define-syntax-rule (define-type+ id ([id* c*] ...) t)
  (begin (define-type id t)
	 (define-type id* (c* id)) ...))

#| In order to simulate the ability to pass the wrong
   number of arguments to a function I need a fold-right
   that takes the shorter of two lists
|#

(: fold-2-left 
   (All (a b c) 
	(-> (-> a b c c) c (Listof a) (Listof b) c)))
(define (fold-2-left p acc l0 l1)
  (if (or (null? l0) (null? l1))
      acc
      (fold-2-left p
		   (p (car l0) (car l1) acc)
		   (cdr l0)
		   (cdr l1))))

#| Some helpers for debuging |#

(: traces (Parameter (Option (Listof Symbol))))
(define traces (make-parameter #f))

(: trace (-> (U Symbol (Listof Symbol)) Void))
(define (trace t)
  (let* ((t* (traces))
         (t* (if t* t* '())))
    (if (symbol? t)
        (traces (cons t t*))
        (traces (append t t*)))))

(define-syntax-rule (trace? s ...)
  (let ([t*? (traces)])
    (and t*? (if (or (memq s t*?) ...) #t #f))))


(: current-log-port (Parameter Output-Port))
(define current-log-port (make-parameter (current-error-port)))

(define-syntax-rule (logf fmt a ...)
  (fprintf (current-log-port) fmt a ...))

;; allows for conditional compilation
(define-for-syntax under-construction?
  (and (getenv "schmlUnderConstruction") #t))

(define-for-syntax syntax-id
  (syntax-rules ()
    [(_ x ...) (begin x ...)]))

(define-for-syntax syntax-void
  (syntax-rules ()
    [(_ x ...) (void)]))

;; An error a syntax tranformer that reports the current macro is undefined
(define-for-syntax syntax-undefined-if-used
  (lambda (stx)
    (syntax-case stx ()
      [(a x ...) (raise-syntax-error 'if-in-construction
                                     "debug macro left in code"
                                     #'(a x ...)
                                     #'a)])))

;; only run this code if we are working on schml
(define-syntax if-in-construction
  (if under-construction?
      syntax-id
      syntax-void))    

(if-in-construction
 (provide exn:todo)
 (struct exn:todo exn ()))


(define-syntax TODO
  (if under-construction?
      (syntax-rules ()
        [(_ x ...)
         (raise 
          (exn:todo
           (format "TODO: ~a" '(x ...))
           (current-continuation-marks)))])
      syntax-undefined-if-used))

