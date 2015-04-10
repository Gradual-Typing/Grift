#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/casts/convert-closures                                               |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass sets up the structure of closures
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require schml/src/helpers
         schml/src/errors
         schml/src/language)

(provide convert-closures)

(: convert-closures (Cast5-Lang Config . -> . Cast6-Lang))
(define (convert-closures prgm conf)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let-values ([(exp count) (run-state (cc-expr exp) count)])
      (Prog (list name count type) exp))))

(define-type BP* C6-Bnd-Procedure*)
(define-type BC* C6-Bnd-Closure*)
(define-type BD* C6-Bnd-Data*)

(: cc-expr (-> C5-Expr (State Nat C6-Expr)))
(define (cc-expr e)
  (match e
    [(Letrec b* e)
     (do (bind-state : (State Nat C6-Expr))
         (bp.bc : (Pair BP* BC*) <- (cc-bnd-lambda* b*))
         (e : C6-Expr <- (cc-expr e))
         (return-state (LetP (car bp.bc) (LetC (cdr bp.bc) e))))]
    [(Let b* e)
     (do (bind-state : (State Nat C6-Expr))
         (b* : BD* <- (cc-bnd-data* b*))
         (e  : C6-Expr <- (cc-expr e))
         (return-state (Let b* e)))]
    [(If t c a)
     (do (bind-state : (State Nat C6-Expr))
         (t : C6-Expr <- (cc-expr t))
         (c : C6-Expr <- (cc-expr c))
         (a : C6-Expr <- (cc-expr a))
         (return-state (If t c a)))]
    [(App e e*)
     (do (bind-state : (State Nat C6-Expr))
         (e  : C6-Expr  <- (cc-expr  e))
         (e* : C6-Expr* <- (cc-expr* e*))
         (if (Var? e)
             ;; this seems really stupid right now but I think this
             ;; is keeping me from making a common minstate whenever doing optimize
             ;; self-reference
             (return-state (App (cons e e) e*))
             (do (bind-state : (State Nat C6-Expr))
                 (id : Uid <- (uid-state "tmp_clos"))
                 (let ([var (Var id)])
                   (return-state
                    (Let (list (cons id e))
                         (App (cons var var) e*)))))))]
    [(Op p e*)
     (do (bind-state : (State Nat C6-Expr))
         (e* : C6-Expr* <- (cc-expr* e*))
         (return-state (Op p e*)))]
    [(Var i)   (return-state (Var i))]
    [(Quote k) (return-state (Quote k))]
    [(Tag t)   (return-state (Tag t))]
    ;; Observables Representation
    [(Blame e)
     (do (bind-state : (State Nat C6-Expr))
         (e : C6-Expr <- (cc-expr e))
         (return-state (Blame e)))]
    [(Observe e t)
     (do (bind-state : (State Nat C6-Expr))
         (e : C6-Expr <- (cc-expr e))
         (return-state (Observe e t)))]
    ;; Type Representation
    [(Type t) (return-state (Type t))]
    [(Type-tag e) (lift-state (inst Type-tag C6-Expr) (cc-expr e))]
    [(Type-Fn-ref e i)
     (do (bind-state : (State Nat C6-Expr))
         (e  : C6-Expr <- (cc-expr  e))
         (return-state (Type-Fn-ref e i)))]
    ;; Dynamic Representation
    [(Dyn-tag e)
     (do (bind-state : (State Nat C6-Expr))
         (e : C6-Expr <- (cc-expr e))
         (return-state (Fn-Caster e)))]
    [(Dyn-immediate e)
     (do (bind-state : (State Nat C6-Expr))
         (e : C6-Expr <- (cc-expr e))
         (return-state (Dyn-immediate e)))]
    [(Dyn-type e)
     (do (bind-state : (State Nat C6-Expr))
         (e : C6-Expr <- (cc-expr e))
         (return-state (Dyn-type e)))]
    [(Dyn-value e)
     (do (bind-state : (State Nat C6-Expr))
         (e : C6-Expr <- (cc-expr e))
         (return-state (Dyn-value e)))]
    [(Dyn-make e1 e2)
     (do (bind-state : (State Nat C6-Expr))
         (e1 : C6-Expr <- (cc-expr e1))
         (e2 : C6-Expr <- (cc-expr e2))
         (return-state (Dyn-make e1 e2)))]
    ;; Function Representation Primitives
    [(Fn-Caster e)
     (do (bind-state : (State Nat C6-Expr))
         (e  : C6-Expr <- (cc-expr  e))
         (return-state (Fn-Caster e)))]
    ;; control flow for effects
    [(Begin e* e)
     (do (bind-state : (State Nat C6-Expr))
         (e* : C6-Expr* <- (cc-expr* e*))
         (e  : C6-Expr  <- (cc-expr  e))
         (return-state (Begin e* e)))]
    ;; Gaurded Representation
    [(GRep-proxied? e)
     (lift-state (inst GRep-proxied? C6-Expr) (cc-expr e))]
    [(UGbox e)
     (lift-state (inst UGbox C6-Expr) (cc-expr e))]
    [(UGbox-ref e)
     (lift-state (inst UGbox-ref C6-Expr) (cc-expr e))]
    [(UGbox-set! e1 e2)
     (do (bind-state : (State Nat C6-Expr))
         (e1 : C6-Expr <- (cc-expr e1))
         (e2 : C6-Expr <- (cc-expr e2))
         (return-state (UGbox-set! e1 e2)))]
    [(Gproxy e1 e2 e3 e4)
     (do (bind-state : (State Nat C6-Expr))
         (e1 : C6-Expr <- (cc-expr e1))
         (e2 : C6-Expr <- (cc-expr e2))
         (e3 : C6-Expr <- (cc-expr e3))
         (e4 : C6-Expr <- (cc-expr e4))
         (return-state (Gproxy e1 e2 e3 e4)))]
    [(Gproxy-for e)
     (lift-state (inst Gproxy-for C6-Expr) (cc-expr e))]
    [(Gproxy-from e)
     (lift-state (inst Gproxy-from C6-Expr) (cc-expr e))]
    [(Gproxy-to e)
     (lift-state (inst Gproxy-to C6-Expr) (cc-expr e))]
    [(Gproxy-blames e)
     (lift-state (inst Gproxy-blames C6-Expr) (cc-expr e))]))

(: cc-expr* (-> C5-Expr* (State Nat C6-Expr*)))
(define (cc-expr* exp*) (map-state cc-expr exp*))

(: cc-bnd-lambda* (-> C5-Bnd-Lambda* (State Nat (Pair BP* BC*))))
(define (cc-bnd-lambda* b*)
  (if (null? b*)
      (return-state (ann (cons '() '()) (Pair BP* BC*)))
      (do (bind-state : (State Nat (Pair BP* BC*)))
          (match-let ([(cons (cons id (Lambda f* (Free c? fv* e))) b*) b*])
            (e : C6-Expr <- (cc-expr e))
            (u1 : Uid <- (clos-uid id))
            (u2 : Uid <- (code-uid id))
            (let* ([proc (Procedure u1 f* c? fv* e)]
                   [bp   (cons u2 proc)]
                   [clos (Closure-Data u2 c? fv*)]
                   [bc   (cons u1 clos)])
              (bp*.bc* : (Pair BP* BC*) <- (cc-bnd-lambda* b*))
              (let* [(bp* : BP* (car bp*.bc*))
                     (bc* : BC* (cdr bp*.bc*))
                     (bp* : BP* (cons bp bp*))
                     (bc* : BC* (cons bc bc*))
                     (bp*.bc* : (Pair BP* BC*) (cons bp* bc*))]
                (return-state bp*.bc*)))))))

(: cc-bnd-data* (-> C5-Bnd-Data* (State Nat C6-Bnd-Data*)))
(define (cc-bnd-data* bnd*) (map-state cc-bnd-data bnd*))

(: cc-bnd-data (-> C5-Bnd-Data (State Nat C6-Bnd-Data)))
(define (cc-bnd-data bnd)
  (do (bind-state : (State Nat C6-Bnd-Data))
      (e : C6-Expr <- (cc-expr (cdr bnd)))
      (return-state (cons (car bnd) e))))

(define-syntax-rule ( u s next)
  (next-uid (string-append (Uid-prefix u) s) next))

(: code-uid (-> Uid (State Nat Uid)))
(define (code-uid u)
  (uid-state (string-append (Uid-prefix u) "_code")))

(: clos-uid (-> Uid (State Nat Uid)))
(define (clos-uid u)
  (uid-state (string-append (Uid-prefix u) "_clos")))
