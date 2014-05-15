#lang racket
(provide (all-defined-out))

(struct Dyn ())
(struct Bool ())
(struct Int ())
(struct Function (from to))
;; Not that allocation takes up to much space but you only ever need
;; one of each of these types because they have no fields
(define Dyn-Type (Dyn))
(define Bool-Type (Bool))
(define Int-Type (Int))
(define Relop-Int-Type
  (Function (vector-immutable Int-Type Int-Type) Bool-Type))
(define Binop-Int-Type
  (Function (vector-immutable Int-Type Int-Type) Int-Type))

(define (type? x)
  (or (Dyn? x) (Bool? x) (Int? x)
      (and (Function? x)
           (vector? (Function-from x))
           (sequence-andmap type? (in-vector (Function-from x)))
           (type? (Function-to x)))))

(define (consistent? t g types)
  (or (Dyn? t) (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (and (Function? t) (Function? g)
           (let ((t-from (Function-from t))
                 (g-from (Function-from g))
                 (t-to (Function-to t))
                 (g-to (Function-to t)))
             (let ((t-arrity (vector-length t-from))
                   (g-arrity (vector-length g-from)))
               (and (= t-arrity g-arrity)
                    (let loop ((i 0))
                      (or (and (= i t-arrity) (consistent? t-to g-to)) 
                          (and (consistent? (vector-ref t-from i)
                                            (vector-ref g-from i))
                               (loop (add1 i)))))))))))

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



