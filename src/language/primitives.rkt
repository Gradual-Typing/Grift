#lang typed/racket/base

(require "./forms.rkt")
(provide (all-defined-out))

;; We should believe in the type system more and do away with the
;; tracking of the type of primitives

(define-type Schml-Primitive (U Schml-Prim Schml-Prim!))
(define-predicate schml-primitive? Schml-Primitive)

(define-type Schml-Prim
  (U Int->Int-Primitive
     IntxInt->Int-Primitive
     IntxInt->Bool-Primitive
     ->Int-Primitive
     FloatxFloat->Float-Primitive
     FloatxFloat->Int-Primitive
     Float->Float-Primitive
     ->Float-Primitive
     Int->Float-Primitive
     Float->Int-Primitive
     FloatxFloat->Bool-Primitive
     Bool->Bool-Primitive
     BoolxBool->Bool-Primitive
     Char->Int-Primitive
     Int->Char-Primitive 
     ->Char-Primitive))

(define-predicate schml-prim? Schml-Prim)

(define-type Schml-Prim!
  (U Timer-Primitive
     Char->Unit-Primitive
     Int->Unit-Primitive
     FloatxInt->Unit-Primitive))

(define-predicate schml-prim!? Schml-Prim!)
#;(: schml-prim!? (Any -> Boolean : Schml-Prim!))
#;
(define (schml-prim!? x)
  (or (timer-primitive? x)))

(define-type Int->Int-Primitive (U 'binary-not))
(define-predicate Int->Int-primitive? Int->Int-Primitive)

(define-type IntxInt->Int-Primitive (U '* '+ '-
                                       'binary-and 'binary-or 'binary-xor 
                                       '%/ '%>> '%<< '%%
                                       'quotient))

(define-type IxI->I-Prim IntxInt->Int-Primitive)

(define-predicate IntxInt->Int-primitive? IntxInt->Int-Primitive)

(define-type ->Int-Primitive (U 'read-int))
(define-type Int->Unit-Primitive (U 'print-int))
(define-type ->I-Prim ->Int-Primitive)

(define-predicate ->Int-primitive? ->Int-Primitive)

(define-type IntxInt->Bool-Primitive (U '< '<= '= '> '>=))
(define-type BoolxBool->Bool-Primitive (U 'and 'or))
(define-type Bool->Bool-Primitive (U 'not))
(define-type IxI->B-Prim IntxInt->Bool-Primitive)
(define-predicate IntxInt->Bool-primitive? IntxInt->Bool-Primitive)
(define-predicate BoolxBool->Bool-primitive? BoolxBool->Bool-Primitive)
(define-predicate Bool->Bool-Primitive? Bool->Bool-Primitive)


(define-type FloatxFloat->Float-Primitive
  (U 'fl+ 'fl- 'fl* 'fl/ 'flmodulo 'flexpt 'flmin 'flmax))
(define-type FloatxFloat->Int-Primitive (U 'flquotient))
(define-type Float->Float-Primitive
  (U 'flabs 'flround 'flfloor 'flceiling 'fltruncate
     'flsin 'flcos 'fltan 'flasin 'flacos 'flatan
     'fllog 'flexp 'flsqrt
     'flmin 'flmax 'flnegate))
(define-type FloatxInt->Unit-Primitive (U 'print-float))
;(define-type Float->Unit-Primitive (U 'print-float))

(define-type ->Float-Primitive    (U 'read-float))
(define-type Int->Float-Primitive (U 'int->float))
(define-type Float->Int-Primitive (U 'float->int))
(define-type FloatxFloat->Bool-Primitive
  (U 'fl< 'fl<= 'fl= 'fl>= 'fl>))

(define-type ->Char-Primitive (U 'read-char))
(define-type Int->Char-Primitive (U 'int->char))
(define-type Char->Int-Primitive (U 'char->int))
(define-type Char->Unit-Primitive (U 'print-char 'display-char))

(define-predicate FloatxFloat->Float-primitive? FloatxFloat->Float-Primitive)
(define-predicate Float->Float-primitive? Float->Float-Primitive)
(define-predicate ->Float-primitive? ->Float-Primitive)
(define-predicate Int->Float-primitive? Int->Float-Primitive)
(define-predicate Float->Int-primitive? Float->Int-Primitive)
(define-predicate FloatxFloat->Bool-primitive? FloatxFloat->Bool-Primitive)
(define-predicate BoolxBool->Bool-Primitive? BoolxBool->Bool-Primitive)
(define-predicate FloatxFloat->Int-Primitive? FloatxFloat->Int-Primitive)

(define-type Timer-Primitive (U 'timer-start 'timer-stop 'timer-report))

(: timer-primitive? (Any -> Boolean : Timer-Primitive))
(define (timer-primitive? x)
  (or (eq? 'timer-start  x)
      (eq? 'timer-stop   x)
      (eq? 'timer-report x)))


(define INTxINT-TYPE (list INT-TYPE INT-TYPE))
(define BOOLxBOOL-TYPE (list BOOL-TYPE BOOL-TYPE))
(define ->INT-TYPE (Fn 0 '() INT-TYPE))
(define INT->INT-TYPE (Fn 1 (list INT-TYPE) INT-TYPE))
(define INTxINT->INT-TYPE (Fn 2 INTxINT-TYPE INT-TYPE))
(define INTxINT->BOOL-TYPE (Fn 2 INTxINT-TYPE BOOL-TYPE))
(define BOOL->BOOL-TYPE (Fn 1 (list BOOL-TYPE) BOOL-TYPE))
(define BOOLxBOOL->BOOL-TYPE (Fn 2 BOOLxBOOL-TYPE BOOL-TYPE))


(define FLOATxFLOAT-TYPE (list FLOAT-TYPE FLOAT-TYPE))
(define FLOATxFLOAT->BOOL-TYPE (Fn 2 FLOATxFLOAT-TYPE BOOL-TYPE))
(define FLOATxFLOAT->INT-TYPE (Fn 2 FLOATxFLOAT-TYPE INT-TYPE))
(define FLOATxFLOAT->FLOAT-TYPE (Fn 2 FLOATxFLOAT-TYPE FLOAT-TYPE))
(define FLOAT->FLOAT-TYPE (Fn 1 (list FLOAT-TYPE) FLOAT-TYPE))
(define ->FLOAT-TYPE (Fn 0 '() FLOAT-TYPE))

(define INT->FLOAT-TYPE (Fn 1 (list INT-TYPE) FLOAT-TYPE))
(define FLOAT->INT-TYPE (Fn 1 (list FLOAT-TYPE) INT-TYPE))

(define ->UNIT-TYPE (Fn 0 '() UNIT-TYPE))



(define ->CHAR-TYPE (Fn 0 '() CHAR-TYPE))
(define INT->CHAR-TYPE (Fn 1 (list INT-TYPE) CHAR-TYPE))
(define CHAR->INT-TYPE (Fn 1 (list CHAR-TYPE) INT-TYPE))


(define INT->UNIT-TYPE (Fn 1 (list INT-TYPE) UNIT-TYPE))
(define CHAR->UNIT-TYPE (Fn 1 (list CHAR-TYPE) UNIT-TYPE))
(define FLOAT->UNIT-TYPE (Fn 1 (list FLOAT-TYPE) UNIT-TYPE))
(define FLOATxINT->UNIT-TYPE (Fn 2 (list FLOAT-TYPE INT-TYPE) UNIT-TYPE))

(define schml-primitive-type-table
  : (HashTable Schml-Primitive (Fn Index (Listof Base-Type) Base-Type))
  (make-immutable-hash
   `((char->int  . ,CHAR->INT-TYPE)
     (int->char  . ,INT->CHAR-TYPE)
     (print-char . ,CHAR->UNIT-TYPE)
     (display-char . ,CHAR->UNIT-TYPE)
     (read-char  . ,->CHAR-TYPE)
     ;; Fixnum operations
     (* . ,INTxINT->INT-TYPE)
     (+ . ,INTxINT->INT-TYPE)
     (- . ,INTxINT->INT-TYPE)
     (%/ . ,INTxINT->INT-TYPE)
     (%% . ,INTxINT->INT-TYPE)
     (%>> . ,INTxINT->INT-TYPE)
     (%<< . ,INTxINT->INT-TYPE)
     (binary-and . ,INTxINT->INT-TYPE)
     (binary-or  . ,INTxINT->INT-TYPE)
     (binary-xor . ,INTxINT->INT-TYPE)
     (binary-not . ,INT->INT-TYPE)
     (read-int . ,->INT-TYPE)
     (print-int . ,INT->UNIT-TYPE)
     (<  . ,INTxINT->BOOL-TYPE)
     (<= . ,INTxINT->BOOL-TYPE)
     (=  . ,INTxINT->BOOL-TYPE)
     (>  . ,INTxINT->BOOL-TYPE)
     (>= . ,INTxINT->BOOL-TYPE)
     (and . ,BOOLxBOOL->BOOL-TYPE)
     (or  . ,BOOLxBOOL->BOOL-TYPE)
     (not . ,BOOL->BOOL-TYPE)
     (quotient . ,INTxINT->INT-TYPE)
     ;; Float operations
     (fl+   . ,FLOATxFLOAT->FLOAT-TYPE)
     (fl-   . ,FLOATxFLOAT->FLOAT-TYPE)
     (fl*   . ,FLOATxFLOAT->FLOAT-TYPE)
     (fl/   . ,FLOATxFLOAT->FLOAT-TYPE)
     (flmodulo . ,FLOATxFLOAT->FLOAT-TYPE)
     (flmin . ,FLOATxFLOAT->FLOAT-TYPE)
     (flmax . ,FLOATxFLOAT->FLOAT-TYPE)
     (flabs . ,FLOAT->FLOAT-TYPE)
     (fl<   . ,FLOATxFLOAT->BOOL-TYPE)
     (fl<=  . ,FLOATxFLOAT->BOOL-TYPE)
     (fl=   . ,FLOATxFLOAT->BOOL-TYPE)
     (fl>=  . ,FLOATxFLOAT->BOOL-TYPE)
     (fl>   . ,FLOATxFLOAT->BOOL-TYPE)
     (flmin . ,FLOATxFLOAT->FLOAT-TYPE)
     (flmax . ,FLOATxFLOAT->FLOAT-TYPE)
     (flnegate . ,FLOAT->FLOAT-TYPE)
     (flround . ,FLOAT->FLOAT-TYPE)
     (flfloor . ,FLOAT->FLOAT-TYPE)
     (flceiling . ,FLOAT->FLOAT-TYPE)
     (fltruncate . ,FLOAT->FLOAT-TYPE)
     (flquotient . ,FLOATxFLOAT->INT-TYPE)
     ;; Float operations (trig)
     (flsin . ,FLOAT->FLOAT-TYPE)
     (flcos .  ,FLOAT->FLOAT-TYPE)
     (fltan .  ,FLOAT->FLOAT-TYPE)
     (flasin . ,FLOAT->FLOAT-TYPE)
     (flacos . ,FLOAT->FLOAT-TYPE)
     (flatan . ,FLOAT->FLOAT-TYPE)
     ;; Float operations (math)
     (fllog  . ,FLOAT->FLOAT-TYPE)
     (flexp  . ,FLOAT->FLOAT-TYPE)
     (flsqrt . ,FLOAT->FLOAT-TYPE)
     (flexpt . ,FLOATxFLOAT->FLOAT-TYPE)
     (float->int . ,FLOAT->INT-TYPE)
     (int->float . ,INT->FLOAT-TYPE)
     (read-float . ,->FLOAT-TYPE)
     (print-float . ,FLOATxINT->UNIT-TYPE) 
     (timer-start . ,->UNIT-TYPE)
     (timer-stop . ,->UNIT-TYPE)
     (timer-report . ,->UNIT-TYPE))))


(: schml-primitive->type
   (-> Schml-Primitive (Fn Index (Listof Base-Type) Base-Type)))
(define (schml-primitive->type p)
  (define (err) (error 'schml-primitive->type "invalid: ~a" p))
  (hash-ref schml-primitive-type-table p err))


(define-type Dyn-Repr-Ctor
  (U 'make))
(define-type Dyn-Repr-Access
  (U 'value
     'type
     'immediate-value
     'immediate-tag
     'box-value
     'box-type))
(define-type Dyn-Repr-Pred
  (U 'immediate-tag=?))

(define-syntax-rule (dyn-make$ value type)
  (Construct DYN-TYPE 'make (list value type)))
(define-syntax-rule (dyn-value$ v)
  (Access DYN-TYPE 'value v #f))
(define-syntax-rule (dyn-type$ v)
  (Access DYN-TYPE 'type v #f))
(define-syntax-rule (dyn-immediate-value$ v)
  (Access DYN-TYPE 'immediate-value v #f))
(define-syntax-rule (dyn-immediate-tag$ v)
  (Access DYN-TYPE 'immediate-tag v #f))
(define-syntax-rule (dyn-immediate-tag=?$ v t)
  (Check DYN-TYPE 'immediate-tag=? v (list t)))
(define-syntax-rule (dyn-box-value$ v)
  (Access DYN-TYPE 'box-value v #f))
(define-syntax-rule (dyn-box-type$ v)
  (Access DYN-TYPE 'box-type v #f))


#|
(define-type Dyn-Repr-Op
  (U 'fn-app
     'tuple-ref
     'pbox-ref
     'pbox-set!
     'pvec-len
     'pvec-ref
     'pvec-set!
     'mbox-ref
     'mbox-set!
     'mvec-ref
     'mvec-set!))

(define-syntax-rule (dyn-fn-app$ e e* t l)
  (Do (Dyn) 'fn-app e (append e* (list t l))))
(define-syntax-rule (dyn-tuple-ref$ e i l)
  (Do (Dyn) 'tuple-ref e (list i l)))
(define-syntax-rule (dyn-pbox-ref$ e)
  (Do (Dyn) 'pbox-ref e '()))
(define-syntax-rule (dyn-pbox-set!$ e v)
  (Do (Dyn) 'pbox-set! e (list v)))
|#

#|-----------------------------------------------------------------------------
We are going to UIL
-----------------------------------------------------------------------------|#

(define-type UIL-Prim  (U Schml-Prim Array-Prim Types-Prim))
(define-type UIL-Prim! (U Schml-Prim! Array-Prim! Print-Prim! Bottom-Prim))
(define-predicate uil-prim-effect? UIL-Prim!)
(define-predicate uil-prim-value? UIL-Prim)

(define-type UIL-Expr-Prim
  (U ->Float-Primitive Float->Float-Primitive Float->Int-Primitive
     FloatxFloat->Float-Primitive
     FloatxFloat->Int-Primitive
     BoolxBool->Bool-Primitive
     Bool->Bool-Primitive
     Int->Int-Primitive
     Int->Float-Primitive Array-Prim IxI->I-Prim ->I-Prim
     ->Char-Primitive Char->Int-Primitive Int->Char-Primitive
     Types-Prim))

(define-type UIL-Pred-Prim (U FloatxFloat->Bool-Primitive
                              IntxInt->Bool-Primitive))

(define-predicate uil-prim-pred? UIL-Pred-Prim)

(define-type Array-Prim (U 'Alloc 'Array-ref))
(define-type Array-Prim! 'Array-set!)
(define-type Types-Prim (U 'Types-hashcons! 'Types-gen-index!))
(define-type Print-Prim! (U 'Printf 'Print 'print-float
                            'print-int 'print-char 'display-char))
(define-type Bottom-Prim (U 'Exit))

(define-type (UIL-Op E) (Op UIL-Prim (Listof E)))
(define-type (UIL-Op! E) (Op UIL-Prim! (Listof E)))
