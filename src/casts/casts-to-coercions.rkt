#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/cast->coercions                                                |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
 Discription: This pass translates casts in the AST to their corresponding
coercions. The output type doesn't allow casts in the AST but is a subset of
future languages that do. This is because this pass is optional and the compiler
should be able to compile programs this the twosome casts for future comparison.
+-------------------------------------------------------------------------------+
|Input Grammar Cast0-Language                                                   |
|Output Grammar Coercion-Language                                                  |
+------------------------------------------------------------------------------|#
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/cast0.rkt"
         "../language/coercion.rkt")

(provide casts->coercions
         make-coercion
         (all-from-out "../language/cast0.rkt"
                       "../language/coercion.rkt"))

(define space-efficient? (make-parameter #t))
(define optimize-first-order-coercions? (make-parameter #t))

;; The entry point for this pass it is called by impose-casting semantics
(: casts->coercions (Cast0-Lang . -> . Coercion-Lang))
(define (casts->coercions prgm)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let ([exp (c2c-expr exp)])
      (Prog (list name next type) exp))))

(: make-coercion (->* (String) (Boolean) (Schml-Type Schml-Type -> Schml-Coercion)))
(define ((make-coercion lbl
                      [space-efficient-normal-form?
                       (and (space-efficient?)
                            (not (optimize-first-order-coercions?)))])
         t1 t2)
  (define recur (make-coercion lbl (space-efficient?)))
  (logging make-coercion () "t1 ~a\n\t t2 ~a\n" t1 t2)
  (define result : Schml-Coercion
    (match* (t1 t2)
      [(t        t) IDENTITY]
      [((Dyn)    t)
       (if space-efficient-normal-form?
           (Sequence (Project t lbl) (Identity))
           (Project t lbl))]
      [(t    (Dyn))
       (if space-efficient-normal-form?
           (Sequence (Identity) (Inject t))
           (Inject t))]
      [((Fn n1 a1* r1) (Fn n2 a2* r2)) #:when (= n1 n2)
       ;; The arity check here means that all coercions have
       ;; the correct arrity under the static type system.
       ;; Notice that the argument types are reversed
       ;; because of contravarience of functions.
       (Fn n1 (map recur a2* a1*) (recur r1 r2))]
      ;; It seems like we could get by without other coercions
      [((GRef t1) (GRef t2))
       (Ref (recur t1 t2) (recur t2 t1))]
      [((GVect t1) (GVect t2))
       (Ref (recur t1 t2) (recur t2 t1))]
      [((STuple n1 t1*) (STuple n2 t2*)) #:when (= n1 n2)
       (CTuple n1 (map recur t1* t2*))]
      [(_ _) (Failed lbl)]))
  (logging make-coercion () "t1 ~a\n\tt2 ~a\n\tresult ~a\n" t1 t2 result)
  result)


;; Fold through the expression converting casts to coercions
(: c2c-expr (C0-Expr -> Crcn-Expr))
(define (c2c-expr exp)
  (match exp
    ;; The only Interesting Case
    [(Cast (app c2c-expr exp) (Twosome t1 t2 lbl))
     (Cast exp (Coercion ((make-coercion lbl) t1 t2)))]
    ;; Everything else should be really boring
    [(Lambda f* (app c2c-expr exp))
     (Lambda f* exp)]
    [(Letrec bnd* exp)
     (Letrec (c2c-bnd* bnd*) (c2c-expr exp))]
    [(Let bnd* exp)
     (Let (c2c-bnd* bnd*) (c2c-expr exp))]
    [(App exp exp*)
     (App (c2c-expr exp) (c2c-expr* exp*))]
    [(Op p exp*)
     (Op p (c2c-expr* exp*))]
    [(If tst csq alt)
     (If (c2c-expr tst) (c2c-expr csq) (c2c-expr alt))]
    [(Switch e c* d)
     (Switch (c2c-expr e) (map-switch-case* c2c-expr c*) (c2c-expr d))]
    [(Begin e* e)
     (Begin (c2c-expr* e*) (c2c-expr e))]
    [(Repeat i e1 e2 a e3 e4)
     (Repeat i (c2c-expr e1) (c2c-expr e2) a (c2c-expr e3) (c2c-expr e4))]
    [(Gbox e)
     (Gbox (c2c-expr e))]
    [(Gunbox e)
     (Gunbox (c2c-expr e))]
    [(Gbox-set! e1 e2)
     (Gbox-set! (c2c-expr e1) (c2c-expr e2))]
    [(Gvector n e)
     (Gvector (c2c-expr n) (c2c-expr e))]
    [(Gvector-ref e i)
     (Gvector-ref (c2c-expr e) (c2c-expr i))]
    [(Gvector-set! e1 e2 e3)
     (Gvector-set! (c2c-expr e1) (c2c-expr e2) (c2c-expr e3))]
    ;; While each of these forms implicitly have coercive behavior
    ;; this will be exposed when we have make coercion
    [(Dyn-Fn-App e e* t* l)
     (Dyn-Fn-App (c2c-expr e) (c2c-expr* e*) t* l)]
    [(Dyn-GRef-Ref e l)
     (Dyn-GRef-Ref (c2c-expr e) l)]
    [(Dyn-GRef-Set! e1 e2 t l)
     (Dyn-GRef-Set! (c2c-expr e1) (c2c-expr e2) t l)]
    [(Dyn-GVector-Ref e i l)
     (Dyn-GVector-Ref (c2c-expr e) (c2c-expr i) l)]
    [(Dyn-GVector-Set! e1 i e2 t l)
     (Dyn-GVector-Set! (c2c-expr e1) (c2c-expr i) (c2c-expr e2) t l)]
    [(Create-tuple e*)
     (Create-tuple (c2c-expr* e*))]
    [(Tuple-proj e i)
     (Tuple-proj (c2c-expr e) i)]
    [(Var id)    (Var id)]
    [(Quote lit) (Quote lit)]))

(: c2c-expr* (C0-Expr* -> Crcn-Expr*))
(define (c2c-expr* e*) (map c2c-expr e*))

;; map c2c-expr through the bindings
(: c2c-bnd* (C0-Bnd* -> Crcn-Bnd*))
(define (c2c-bnd* b*)
  (map
   (lambda ([b : C0-Bnd])
     (cons (car b) (c2c-expr (cdr b))))
   b*))
