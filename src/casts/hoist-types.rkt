#lang typed/racket
#|-----------------------------------------------------------------------------+
|Pass: src/casts/hoist-types                                                   |
+------------------------------------------------------------------------------+
|Author: Deyaaeldeen Almahallawi (dalmahal@indiana.edu)                        |
+------------------------------------------------------------------------------+
|Description: It hoist all types to the top of the program, and put pointer to |
|to their definition in each use, so that allocation of them happens once.     |
+------------------------------------------------------------------------------+
|Input Grammar Cast3-Language                                                  |
|Output Grammar Cast-with-lifted-types-Language                                |
+-----------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../language.rkt")

;; Only the pass is provided by this module
(provide hoist-types)

(: hoist-types (Cast3-Lang Config -> Cast-with-hoisted-types))
(trace-define (hoist-types prgm config)
              (match-let ([(Prog (list name next type) exp) prgm])
                (match-let-values ([(exp (list next ht))
                                    (run-state (ht-expr exp) (list next (mk-cu-map)))])
                                  (Prog (list name next type) (LetT (map flip (hash->list ht)) exp)))))

(define-type CU-Map (HashTable Compact-Type Uid))

(define-type St (List Nat CU-Map))

(: flip ((Pair Compact-Type Uid) -> C/LT-TBnd))
(define (flip x)
  ((inst cons Uid Compact-Type) (cdr x) (car x)))

(: mk-cu-map (-> CU-Map))
(define (mk-cu-map) (hash))

(: st-extend (Compact-Type -> (State St Uid)))
(define ((st-extend t) st)
  (match-let* ([(list next ht) st]
               [uid (Uid "typeid" next)])
    (values uid (list (add1 next)
                      (hash-set ht t uid)))))

(: ht-bnd (C3-Bnd -> (State St C/LT-Bnd)))
(define (ht-bnd exp)
  (do (bind-state : (State St C/LT-Bnd))
      (match-let ([(cons u e) exp])
        (e : C/LT-Expr <- (ht-expr e))
        (return-state (cons u e)))))

(: schml->prim (Schml-Type -> Prim-Type))
(define (schml->prim t)
  (match t
    [(Dyn) DYN-TYPE]
    [(Unit) UNIT-TYPE]
    [(Int) INT-TYPE]
    [(Bool) BOOL-TYPE]))

(: identify-types (Schml-Type -> (State St Prim-Type)))
(define (identify-types t)
  (logging identify-types (All) "~v" t)
  (do (bind-state : (State St Prim-Type))
      (match t
        [(Dyn) (return-state DYN-TYPE)]
        [(? base-type?) (return-state (schml->prim t))]
        [(Fn ar t* t)
         (t* : (Listof Prim-Type) <- (map-state identify-types t*))
         (t : Prim-Type <- (identify-types t))
         (identify-types-helper (Fn ar t* t))]
        [(GRef t)
         (t1 : Prim-Type <- (identify-types t))
         (identify-types-helper (GRef t1))]
        [(MRef t)
         (t1 : Prim-Type <- (identify-types t))
         (identify-types-helper (MRef t1))]
        [(GVect t)
         (t1 : Prim-Type <- (identify-types t))
         (identify-types-helper (GVect t1))]
        [(MVect t)
         (t1 : Prim-Type <- (identify-types t))
         (identify-types-helper (MVect t1))])))

(: identify-types-helper (Compact-Type -> (State St Prim-Type)))
(define (identify-types-helper t)
  (do (bind-state : (State St Prim-Type))
      (s : St <- get-state)
    (match-let ([(list next ht) s])
      (let ([uid? (hash-ref ht t #f)])
        (if uid?
            (return-state (TypeId uid?))
            (do (bind-state : (State St Prim-Type))
                (uid : Uid <- (st-extend t))
                (return-state (TypeId uid))))))))

(: ht-expr (C3-Expr -> (State St C/LT-Expr)))
(define (ht-expr exp)
  (do (bind-state : (State St C/LT-Expr))
      (match exp
        [(Lambda f* (Castable ctr exp))
         (exp : C/LT-Expr <- (ht-expr exp))
         (return-state (Lambda f* (Castable ctr exp)))]
        [(Letrec b* exp)
         (b*  : C/LT-Bnd* <- (map-state ht-bnd b*))
         (exp : C/LT-Expr <- (ht-expr exp))
         (return-state (Letrec b* exp))]
        [(Let b* exp)
         (b*  : C/LT-Bnd* <- (map-state ht-bnd b*))
         (exp : C/LT-Expr <- (ht-expr exp))
         (return-state (Let b* exp))]
        [(App exp exp*)
         (exp  : C/LT-Expr  <- (ht-expr exp))
         (exp* : C/LT-Expr* <- (map-state ht-expr exp*))
         (return-state (App exp exp*))]
        [(Op p exp*)
         (exp* : C/LT-Expr* <- (map-state ht-expr exp*))
         (return-state (Op p exp*))]
        [(If tst csq alt)
         (tst  : C/LT-Expr <- (ht-expr tst))
         (csq  : C/LT-Expr <- (ht-expr csq))
         (alt  : C/LT-Expr <- (ht-expr alt))
         (return-state (If tst csq alt))]
        [(Begin exp* exp)
         (exp* : C/LT-Expr* <- (map-state ht-expr exp*))
         (exp  : C/LT-Expr  <- (ht-expr exp))
         (return-state (Begin exp* exp))]
        [(Repeat i e1 e2 e3)
         (e1 : C/LT-Expr <- (ht-expr e1))
         (e2 : C/LT-Expr <- (ht-expr e2))
         (e3 : C/LT-Expr <- (ht-expr e3))
         (return-state (Repeat i e1 e2 e3))]
        [(Fn-Caster e)
         (e  : C/LT-Expr <- (ht-expr e))
         (return-state (Fn-Caster e))]
        [(Type-tag e)
         (e  : C/LT-Expr <- (ht-expr e))
         (return-state (Type-tag e))]
        [(Type-Fn-arg e i)
         (e  : C/LT-Expr <- (ht-expr e))
         (i  : C/LT-Expr <- (ht-expr i))
         (return-state (Type-Fn-arg e i))]
        [(Type-Fn-return e)
         (e  : C/LT-Expr <- (ht-expr e))
         (return-state (Type-Fn-return e))]
        [(Type-Fn-arity e)
         (e  : C/LT-Expr <- (ht-expr e))
         (return-state (Type-Fn-arity e))]
        [(Type-GRef-to e)
         (e  : C/LT-Expr <- (ht-expr e))
         (return-state (Type-GRef-to e))]
        [(Type-GVect-to e)
         (e  : C/LT-Expr <- (ht-expr e))
         (return-state (Type-GVect-to e))]
        [(Dyn-tag e)
         (e  : C/LT-Expr <- (ht-expr e))
         (return-state (Dyn-tag e))]
        [(Dyn-immediate e)
         (e  : C/LT-Expr <- (ht-expr e))
         (return-state (Dyn-immediate e))]
        [(Dyn-type e)
         (e  : C/LT-Expr <- (ht-expr e))
         (return-state (Dyn-type e))]
        [(Dyn-value e)
         (e  : C/LT-Expr <- (ht-expr e))
         (return-state (Dyn-value e))]
        [(Dyn-make e1 e2)
         (e1 : C/LT-Expr <- (ht-expr e1))
         (e2 : C/LT-Expr <- (ht-expr e2))
         (return-state (Dyn-make e1 e2))]
        [(Blame e)
         (e : C/LT-Expr <- (ht-expr e))
         (return-state (Blame e))]
        [(Observe e t)
         (e : C/LT-Expr <- (ht-expr e))
         (return-state (Observe e t))]
        [(Type t)
         (t : Prim-Type <- (identify-types t))
         (return-state (Type t))]
        [(Tag s) (return-state (Tag s))]
        [(Var i) (return-state (Var i))]
        [(Quote k) (return-state (Quote k))]
        [(UGbox exp)
         (exp  : C/LT-Expr  <- (ht-expr exp))
         (return-state (UGbox exp))]
        [(UGbox-ref exp)
         (exp  : C/LT-Expr  <- (ht-expr exp))
         (return-state (UGbox-ref exp))]
        [(UGbox-set! exp1 exp2)
         (exp1 : C/LT-Expr  <- (ht-expr exp1))
         (exp2 : C/LT-Expr  <- (ht-expr exp2))
         (return-state (UGbox-set! exp1 exp2))]
        [(UGvect exp1 exp2)
         (exp1  : C/LT-Expr  <- (ht-expr exp1))
         (exp2  : C/LT-Expr  <- (ht-expr exp2))
         (return-state (UGvect exp1 exp2))]
        [(UGvect-ref exp1 exp2)
         (exp1  : C/LT-Expr  <- (ht-expr exp1))
         (exp2  : C/LT-Expr  <- (ht-expr exp2))
         (return-state (UGvect-ref exp1 exp2))]
        [(UGvect-set! exp1 exp2 exp3)
         (exp1 : C/LT-Expr  <- (ht-expr exp1))
         (exp2 : C/LT-Expr  <- (ht-expr exp2))
         (exp3 : C/LT-Expr  <- (ht-expr exp3))
         (return-state (UGvect-set! exp1 exp2 exp3))]
        [(GRep-proxied? exp)
         (exp  : C/LT-Expr  <- (ht-expr exp))
         (return-state (GRep-proxied? exp))]
        [(Gproxy e1 e2 e3 e4)
         (e1 : C/LT-Expr  <- (ht-expr e1))
         (e2 : C/LT-Expr  <- (ht-expr e2))
         (e3 : C/LT-Expr  <- (ht-expr e3))
         (e4 : C/LT-Expr  <- (ht-expr e4))
         (return-state (Gproxy e1 e2 e3 e4))]
        [(Gproxy-for exp)
         (exp  : C/LT-Expr  <- (ht-expr exp))
         (return-state (Gproxy-for exp))]
        [(Gproxy-from exp)
         (exp  : C/LT-Expr  <- (ht-expr exp))
         (return-state (Gproxy-from exp))]
        [(Gproxy-to exp)
         (exp  : C/LT-Expr  <- (ht-expr exp))
         (return-state (Gproxy-to exp))]
        [(Gproxy-blames exp)
         (exp  : C/LT-Expr  <- (ht-expr exp))
         (return-state (Gproxy-blames exp))])))
