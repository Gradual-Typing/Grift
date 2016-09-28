#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/cast->coercions                                                |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
Description: This pass translates casts in the AST to their corresponding
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
         "../language/cast-with-pure-letrec.rkt"
         "../language/coercion.rkt")

(provide casts->coercions mk-coercion
         (all-from-out "../language/cast-with-pure-letrec.rkt"
                       "../language/coercion.rkt"))

;; The entry point for this pass it is called by impose-casting-semantics
(: casts->coercions (Cast/Pure-Letrec Config . -> . Coercion-Lang))
(define (casts->coercions prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let ([exp (c2c-expr exp)])
      (Prog (list name next type) exp))))

(: mk-coercion (Blame-Label ->
                            (Schml-Type Schml-Type -> Schml-Coercion)))
(define ((mk-coercion lbl) t1 t2)
  (define recur (mk-coercion lbl))
  (logging mk-coercion () "t1 ~a\n\t t2 ~a\n" t1 t2)
  (define result : Schml-Coercion
    (cond
      [(equal? t1 t2) (Identity)]
      [(Dyn? t1) (Sequence (Project t2 lbl) (Identity))]
      [(Dyn? t2) (Sequence (Identity) (Inject t1))]
      [else (match* (t1 t2)
              [((Fn n1 a1* r1) (Fn n2 a2* r2)) #:when (= n1 n2)
               ;; The arity check here means that all coercions have
               ;; the correct arity under the static type system.
               ;; Notice that the argument types are reversed
               ;; because of contravarience of functions.
               (Fn n1 (map recur a2* a1*) (recur r1 r2))]
              ;; It seems like we could get by without other coercions
              [((GRef t1) (GRef t2))
               (Ref (recur t1 t2) (recur t2 t1))]
              [((GVect t1) (GVect t2))
               (Ref (recur t1 t2) (recur t2 t1))]
              [((MRef _) (MRef t2)) (MonoRef t2)]
              [(_ _) (Failed lbl)])]))
  (logging mk-coercion () "t1 ~a\n\tt2 ~a\n\tresult ~a\n" t1 t2 result)
  result)

;; Fold through the expression converting casts to coercions
(: c2c-expr (C/PL-Expr -> Crcn-Expr))
(define (c2c-expr exp)
  (match exp
    ;; The only Interesting Case
    [(Cast (app c2c-expr exp) (Twosome t1 t2 lbl))
     (Cast exp (Coercion ((mk-coercion lbl) t1 t2)))]
    ;; Everything else should be really boring
    [(Lambda f* (app c2c-expr exp))
     (Lambda f* exp)]
    [(Letrec bnd* exp)
     (Letrec (c2c-bndl* bnd*) (c2c-expr exp))]
    [(Let bnd* exp)
     (Let (c2c-bnd* bnd*) (c2c-expr exp))]
    [(App exp exp*)
     (App (c2c-expr exp) (c2c-expr* exp*))]
    [(Op p exp*)
     (Op p (c2c-expr* exp*))]
    [(If tst csq alt)
     (If (c2c-expr tst) (c2c-expr csq) (c2c-expr alt))]
    [(Begin e* e)
     (Begin (c2c-expr* e*) (c2c-expr e))]
    [(Repeat i e1 e2 e3)
     (Repeat i (c2c-expr e1) (c2c-expr e2) (c2c-expr e3))]
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
    [(Mbox e t) (Mbox (c2c-expr e) t)]
    [(Munbox e) (Munbox (c2c-expr e))]
    [(Mbox-set! e1 e2) (Mbox-set! (c2c-expr e1) (c2c-expr e2))]
    [(MBoxCastedRef u t)
     (MBoxCastedRef u t)]
    [(MBoxCastedSet! u (app c2c-expr e) t)
     (MBoxCastedSet! u e t)]
    [(Mvector e1 e2 t) (Mvector (c2c-expr e1) (c2c-expr e2) t)]
    [(Mvector-ref e1 e2) (Mvector-ref (c2c-expr e1) (c2c-expr e2))]
    [(Mvector-set! e1 e2 e3)
     (Mvector-set! (c2c-expr e1) (c2c-expr e2) (c2c-expr e3))]
    [(MVectCastedRef u i t)
     (MVectCastedRef u (c2c-expr i) t)]
    [(MVectCastedSet! u (app c2c-expr i) (app c2c-expr e) t)
     (MVectCastedSet! u i e t)]
    [(Var id) (Var id)]
    [(Quote lit) (Quote lit)]))

(: c2c-expr* (C/PL-Expr* -> Crcn-Expr*))
(define (c2c-expr* e*) (map c2c-expr e*))

;; map c2c-expr through the bindings
(: c2c-bnd* (C/PL-Bnd* -> Crcn-Bnd*))
(define (c2c-bnd* b*)
  (map
   (lambda ([b : C/PL-Bnd])
     (cons (car b) (c2c-expr (cdr b))))
   b*))

(: c2c-bndl* (C/PL-Bnd-Lam* -> Crcn-Bnd-Lam*))
(define (c2c-bndl* b*)
  (map
   (lambda ([b : C/PL-Bnd-Lam])
     (match-let ([(cons u (Lambda f* e)) b])
       (cons u (Lambda f* (c2c-expr e)))))
   b*))
