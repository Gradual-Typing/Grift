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

#|
(require (for-syntax  racket/list))

(provide (all-defined-out))

;; empty-env creates a new empty environment
(define (empty-env) (hasheq))

;; env-lookup searches for key in env and returns its assiciated value
;; if no value is and a thunk is provided for default then it is
;; envoked otherwise the value of default is returned.
(define (env-lookup env key dflt) 
  (hash-ref env key dflt))

;; Creating new environments ...
;; env-extend creates a new environment by extending an old
;; environment with a new key value mapping
(define (env-extend env key val) (hash-set env key val))

;; env-extend* creates a new environment by extending an old
;; environment with new key value mappings from a list of keys
;; and a list of values.
(define (env-extend* env k* v*)
  (for/fold ([e env]) ([k (in-list k*)] [v (in-list v*)])
    (hash-set e k v)))

;; env-extend/env creates a new environment by extending an old
;; environment with new key value mappings from another environment
(define (env-extend/env f s)
  (for/fold ([h f]) ([(k v) (in-hash s)]) (hash-set h k v))) 

;;  length=? checks if a list (l) has lenght n.
(define (length=? l n)
  (define (do-it l n) (or (and (pair? l) (positive? n) (do-it (cdr l) (sub1 n)))
                          (and (not (pair? l)) (zero? n))))
  (if (and (number? n) (pair? l))
      (do-it l n)
      (error 'length=? "type mismatch")))

(define (null?-car-cdr-map ls)
  (if (null? ls)
        (values #f #f '() '())
        (let-values (((n? p? c* d*) (null?-car-cdr-map (cdr ls))))
          (if (null? (car ls))
              (values #t p? 'woops '())
              (values n? #t (car ls) (cdr ls))))))

(define-syntax (map/length= stx)
  (syntax-case stx ()
    [(_ p ls ... th)
     (with-syntax (((t ...) (generate-temporaries #'(ls ...)))
                   ((n ...) (generate-temporaries #'(ls ...))))
       #'(letrec ([recur (lambda (t ...)
                             (let ((n (null? t)) ...)
                               (cond
                                 [(and n ...) '()]
                                 [(or n ...) (th)]
                                 [else (cons (p (car t) ...)
                                             (recur (cdr t) ...))])))])
             (recur ls ...)))]))

;;

(define-syntax (andmap/length= stx)
  (syntax-case stx ()
    [(_ p? ls ...)
     (with-syntax (((t ...) (generate-temporaries #'(ls ...))))
       #'(let ([t ls] ...)
           (letrec ([recur (lambda (t ...)
                             (or (and (null? t) ...)
                                 (and (not (or (null? t) ...))
                                      (p? (car t) ...)
                                      (recur (cdr t) ...))))])
             (recur t ...))))]))


;; th is syntax sugar for thunking values
(define-syntax th
  (syntax-rules ()
    ((_ e) (lambda () e))))

(define-syntax (apply/overflow stx)
  (let ((limit (expt 2 64)))
    (syntax-case stx ()
      ((_ o a ...)
       #`(modulo (o a ...) #,limit)))))

(define (mk-struct struct)
  (let-values (((type not-specific) (struct-info struct)))
    (if not-specific
        (error 'mk-struct "could not make a struct of this type ~a" struct)
        (struct-type-make-constructor type))))
        
|#
