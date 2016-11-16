#lang typed/racket/base

(require "./forms.rkt")
(provide (all-defined-out))

;; We should believe in the type system more and do away with the
;; tracking of the type of primitives

(define-type Schml-Primitive (U Schml-Prim Schml-Prim!))
(define-predicate schml-primitive? Schml-Primitive)

(define-type Schml-Prim
  (U IntxInt->Int-Primitive
     IntxInt->Bool-Primitive
     ->Int-Primitive
     FloatxFloat->Float-Primitive
     Float->Float-Primitive
     ->Float-Primitive
     Int->Float-Primitive
     Float->Int-Primitive
     FloatxFloat->Bool-Primitive))

(define-predicate schml-prim? Schml-Prim)

(define-type Schml-Prim!
  (U Timer-Primitive))

(define-predicate schml-prim!? Schml-Prim!)
#;(: schml-prim!? (Any -> Boolean : Schml-Prim!))
#;
(define (schml-prim!? x)
  (or (timer-primitive? x)))

(define-type IntxInt->Int-Primitive (U '* '+ '-
                                       'binary-and 'binary-or 'binary-xor
                                       '%/ '%>> '%<< '%%))

(define-type IxI->I-Prim IntxInt->Int-Primitive)

(define-predicate IntxInt->Int-primitive? IntxInt->Int-Primitive)

(define-type ->Int-Primitive (U 'read-int))
(define-type ->I-Prim ->Int-Primitive)

(define-predicate ->Int-primitive? ->Int-Primitive)

(define-type IntxInt->Bool-Primitive (U '< '<= '= '> '>=))
(define-type IxI->B-Prim IntxInt->Bool-Primitive)
(define-predicate IntxInt->Bool-primitive? IntxInt->Bool-Primitive)

(define-type FloatxFloat->Float-Primitive
  (U 'fl+ 'fl- 'fl* 'fl/ 'flmodulo 'flexpt))
(define-type Float->Float-Primitive
  (U 'flabs 'flround 'flfloor 'flceiling 'fltruncate
     'flsin 'flcos 'fltan 'flasin 'flacos 'flatan
     'fllog 'flexp 'flsqrt))
(define-type ->Float-Primitive    (U 'read-float))
(define-type Int->Float-Primitive (U 'int->float))
(define-type Float->Int-Primitive (U 'float->int))
(define-type FloatxFloat->Bool-Primitive
  (U 'fl< 'fl<= 'fl= 'fl>= 'fl>))


(define-predicate FloatxFloat->Float-primitive? FloatxFloat->Float-Primitive)
(define-predicate Float->Float-primitive? Float->Float-Primitive)
(define-predicate ->Float-primitive? ->Float-Primitive)
(define-predicate Int->Float-primitive? Int->Float-Primitive)
(define-predicate Float->Int-primitive? Float->Int-Primitive)
(define-predicate FloatxFloat->Bool-primitive? FloatxFloat->Bool-Primitive)

(define-type Timer-Primitive (U 'timer-start 'timer-stop 'timer-report))

(: timer-primitive? (Any -> Boolean : Timer-Primitive))
(define (timer-primitive? x)
  (or (eq? 'timer-start  x)
      (eq? 'timer-stop   x)
      (eq? 'timer-report x)))


(define INTxINT-TYPE (list INT-TYPE INT-TYPE))
(define INTxINT->BOOL-TYPE (Fn 2 INTxINT-TYPE BOOL-TYPE))
(define INTxINT->INT-TYPE (Fn 2 INTxINT-TYPE INT-TYPE))
(define ->INT-TYPE (Fn 0 '() INT-TYPE))

(define FLOATxFLOAT-TYPE (list FLOAT-TYPE FLOAT-TYPE))
(define FLOATxFLOAT->BOOL-TYPE (Fn 2 FLOATxFLOAT-TYPE BOOL-TYPE))
(define FLOATxFLOAT->FLOAT-TYPE (Fn 2 FLOATxFLOAT-TYPE FLOAT-TYPE))
(define FLOAT->FLOAT-TYPE (Fn 1 (list FLOAT-TYPE) FLOAT-TYPE))
(define ->FLOAT-TYPE (Fn 0 '() FLOAT-TYPE))

(define INT->FLOAT-TYPE (Fn 1 (list INT-TYPE) FLOAT-TYPE))
(define FLOAT->INT-TYPE (Fn 1 (list FLOAT-TYPE) INT-TYPE))

(define ->UNIT-TYPE (Fn 0 '() UNIT-TYPE))

(: schml-primitive->type
   (-> Schml-Primitive (Fn Index
                           (Listof (U Float Int Bool))
                           (U Int Bool Unit Float))))
(define (schml-primitive->type p)
  (cond
    [(IntxInt->Bool-primitive? p)       INTxINT->BOOL-TYPE]
    [(IntxInt->Int-primitive? p)        INTxINT->INT-TYPE]
    [(->Int-primitive? p)               ->INT-TYPE]
    [(FloatxFloat->Bool-primitive? p)   FLOATxFLOAT->BOOL-TYPE]
    [(FloatxFloat->Float-primitive? p)  FLOATxFLOAT->FLOAT-TYPE]
    [(Float->Float-primitive? p)        FLOAT->FLOAT-TYPE]
    [(Float->Int-primitive? p)          FLOAT->INT-TYPE]
    [(Int->Float-primitive? p)          INT->FLOAT-TYPE]
    [(->Float-primitive? p)             ->FLOAT-TYPE]
    [(timer-primitive? p)               ->UNIT-TYPE]
    ;; if the all cases are not covered p have members and fail
    ;; to type-check. 
    [else p]))


#|-----------------------------------------------------------------------------
We are going to UIL
-----------------------------------------------------------------------------|#

(define-type UIL-Prim  (U Schml-Prim Array-Prim))
(define-type UIL-Prim! (U Schml-Prim! Array-Prim! Print-Prim! Bottom-Prim))
(define-predicate uil-prim-effect? UIL-Prim!)
(define-predicate uil-prim-value? UIL-Prim)

(define-type UIL-Expr-Prim
  (U ->Float-Primitive Float->Float-Primitive Float->Int-Primitive
     FloatxFloat->Float-Primitive
     Int->Float-Primitive Array-Prim IxI->I-Prim ->I-Prim))

(define-type UIL-Pred-Prim (U FloatxFloat->Bool-Primitive
                              IntxInt->Bool-Primitive))

(define-predicate uil-prim-pred? UIL-Pred-Prim)

(define-type Array-Prim (U 'Alloc 'Array-ref))
(define-type Array-Prim! 'Array-set!)
(define-type Print-Prim! (U 'Printf 'Print 'print-float))
(define-type Bottom-Prim (U 'Exit))

(define-type (UIL-Op E) (Op UIL-Prim (Listof E)))
(define-type (UIL-Op! E) (Op UIL-Prim! (Listof E)))

