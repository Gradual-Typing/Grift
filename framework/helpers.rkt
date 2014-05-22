#lang racket
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
    [(_ p ls ...)
     (with-syntax (((t ...) (generate-temporaries #'(ls ...))))
       #'(let ([t ls] ...)
           (letrec ([recur (lambda (t ...)
                             (if (and (null? t) ...)
                                 '()
                                 (and (or (null? t) ...)
                                      (cons (p (car ls) ...)
                                            (recur (cdr ls) ...)))))])
             (recur t ...))))]))

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
