#lang racket
(require Schml/framework/helpers)
(provide (all-defined-out))

(struct Dyn ())
(struct Bool ())
(struct Int ())
(struct Function (from to) #:transparent)
;; Not that allocation takes up to much space but you only ever need
;; one of each of these types because they have no fields
(define Dyn-Type (Dyn))
(define Bool-Type (Bool))
(define Int-Type (Int))
(define Relop-Int-Type
  (Function `(,Int-Type ,Int-Type) Bool-Type))
(define Binop-Int-Type
  (Function `(,Int-Type ,Int-Type) Int-Type))

(define/match (type? x)
  [((Dyn)) #t]
  [((Bool)) #t]
  [((Int)) #t]
  [((Function (list (? type?) ...) (? type?))) #t]
  [(otherwise) #f])

(define (consistent? t g)
  (or (Dyn? t) (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (function-consistent? t g)))

(define (function-consistent? t g)
  (and (Function? t) (Function? g)
       (andmap/length= consistent? (Function-from t) (Function-from g))
       (consistent? (Function-to t) (Function-to g))))

(define (shallow-consistent? t g)
  (or (Dyn? t) (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (and (Function? t) (Function? g))))

(define (meet t g err)
  (cond
    [(Dyn? t) g]
    [(Dyn? g) t]
    [(and (Int? t) (Int? g)) Int-Type]
    [(and (Bool? t) (Bool? g)) Bool-Type]
    [(and (Function? t) (Function? g))
     (let ((t-from (Function-from t))
           (g-from (Function-from g))
           (t-to (Function-to t))
           (g-to (Function-to t)))
       (let ((arrity (vector-length t-from)))
         (if (= arrity (vector-length g-from))
             (Function (for/vector #:length arrity ([t (in-vector t-from)]
                                                    [g (in-vector g-from)]) 
                                   (meet t g))
                       (meet t-to g-to)) 
             (err "Function type with incorrect arrity"))))]
    [else (err "Types are not consistent")]))




