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

(provide casts->coercions mk-coercion
         (all-from-out "../language/cast0.rkt"
                       "../language/coercion.rkt"))

;; The entry point for this pass it is called by impose-casting semantics
(: casts->coercions (Cast0-Lang Config . -> . Coercion-Lang))
(define (casts->coercions prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let-values ([([exp : Crcn-Expr] [state : C2C-State])
                  (run-state (c2c-expr exp) '())])
      (Prog (list name next type) exp))))

(: mk-coercion (Blame-Label ->
                (Schml-Type Schml-Type -> (Coercion Schml-Type Blame-Label))))
(define ((mk-coercion lbl) t1 t2)
  (define recur (mk-coercion lbl))
  (logging mk-coercion () "t1 ~a\n\t t2 ~a\n" t1 t2)
  (define result : (Coercion Schml-Type Blame-Label)
    (if (equal? t1 t2)
        (Identity t1)
        (match* (t1 t2)
          [((Dyn)    t)  (Sequence (Project t lbl) (Identity t))]
          [(t    (Dyn))  (Sequence (Identity t) (Inject t))]
          [((Fn n1 a1* r1) (Fn n2 a2* r2)) #:when (= n1 n2)
           ;; The arity check here means that all coercions have
           ;; the correct arrity under the static type system.
           ;; Notice that the argument types are reversed
           ;; because of contravarience of functions.
           (Proxy-Fn n1 (map recur a2* a1*) (recur r1 r2))]
          ;; It seems like we could get by without other coercions
          [((GRef t1) (GRef t2))
           (Proxy-Guarded (recur t1 t2) (recur t2 t1))]
          [((GVect t1) (GVect t2))
           (Proxy-Guarded (recur t1 t2) (recur t2 t1))]
          [(_ _) (Failed lbl)])))
  (logging mk-coercion () "t1 ~a\n\tt2 ~a\n\tresult ~a\n" t1 t2 result)
  result)


(define-type C2C-State Null)

;; Fold through the expression converting casts to coercions
(: c2c-expr (C0-Expr -> (State C2C-State Crcn-Expr)))
(define (c2c-expr exp)
  (do (bind-state : (State C2C-State Crcn-Expr))
      ;; All Clauses of the match are still in the do macro
      (match exp
        ;; The only Interesting Case
        [(Cast exp t1 t2 lbl)
         (exp : Crcn-Expr <- (c2c-expr exp))
         (return-state (Coerce ((mk-coercion lbl) t1 t2) exp))]
        ;; Everything else should be really boring
        [(Lambda f* exp)
         (exp : Crcn-Expr <- (c2c-expr exp))
         (return-state (Lambda f* exp))]
        [(Letrec bnd* exp)
         (bnd* : Crcn-Bnd* <- (c2c-bnd* bnd*))
         (exp  : Crcn-Expr <- (c2c-expr exp))
         (return-state (Letrec bnd* exp))]
        [(Let bnd* exp)
         (bnd* : Crcn-Bnd* <- (c2c-bnd* bnd*))
         (exp : Crcn-Expr  <- (c2c-expr exp))
         (return-state (Let bnd* exp))]
        [(App exp exp*)
         (exp  : Crcn-Expr  <- (c2c-expr  exp))
         (exp* : Crcn-Expr* <- (c2c-expr* exp*))
         (return-state (App exp exp*))]
        [(Op p exp*)
         (exp* : Crcn-Expr* <- (c2c-expr* exp*))
         (return-state (Op p exp*))]
        [(If tst csq alt)
         (tst : Crcn-Expr <- (c2c-expr tst))
         (csq : Crcn-Expr <- (c2c-expr csq))
         (alt : Crcn-Expr <- (c2c-expr alt))
         (return-state (If tst csq alt))]
        [(Begin e* e)
         (e* : Crcn-Expr* <- (c2c-expr* e*))
         (e  : Crcn-Expr  <- (c2c-expr  e))
         (return-state (Begin e* e))]
        [(Repeat i e1 e2 e3)
         (e1 : Crcn-Expr <- (c2c-expr e1))
         (e2 : Crcn-Expr <- (c2c-expr e2))
         (e3 : Crcn-Expr <- (c2c-expr e3))
         (return-state (Repeat i e1 e2 e3))]
        [(Gbox e)
         (e : Crcn-Expr <- (c2c-expr e))
         (return-state (Gbox e))]
        [(Gunbox e)
         (e : Crcn-Expr <- (c2c-expr e))
         (return-state (Gunbox e))]
        [(Gbox-set! e1 e2)
         (e1 : Crcn-Expr <- (c2c-expr e1))
         (e2 : Crcn-Expr <- (c2c-expr e2))
         (return-state (Gbox-set! e1 e2))]
        [(Gvector n e)
         (n : Crcn-Expr <- (c2c-expr n))
         (e : Crcn-Expr <- (c2c-expr e))
         (return-state (Gvector n e))]
        [(Gvector-ref e i) 
         (e : Crcn-Expr <- (c2c-expr e))
         (i : Crcn-Expr <- (c2c-expr i))
         (return-state (Gvector-ref e i))]
        [(Gvector-set! e1 i e2)
         (e1 : Crcn-Expr <- (c2c-expr e1))
         (i  : Crcn-Expr <- (c2c-expr i))
         (e2 : Crcn-Expr <- (c2c-expr e2))
         (return-state (Gvector-set! e1 i e2))]
        [(Var id)    (return-state (Var id))]
        [(Quote lit) (return-state (Quote lit))])))

(: c2c-expr* (-> C0-Expr* (State C2C-State Crcn-Expr*)))
(define (c2c-expr* e*) (map-state c2c-expr e*))

;; map c2c-expr through the bindings
(: c2c-bnd* (-> C0-Bnd* (State C2C-State Crcn-Bnd*)))
(define (c2c-bnd* b*)
  (: c2c-bnd (-> C0-Bnd (State C2C-State Crcn-Bnd)))
  (define (c2c-bnd b)
    (match-let ([(cons i e) b])
      (do (bind-state : (State C2C-State Crcn-Bnd))
          (e : Crcn-Expr <- (c2c-expr  e))
          (return-state (cons i e)))))
  (map-state c2c-bnd b*))




