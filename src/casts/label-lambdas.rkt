#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/label-lambdas                                             |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: Moves every lambda into a letrec thus giving it a
| binding. With the new closure conversion steps this pass is now uneeded
| and thus needs to be removed unfortunately it is not entirely evident how to
| do so and I need to make some progress thus letrec-refinement and lambda labeling
| is occuring here. This should be changed. One thought is to perform letrec
| purification earlier. In general I don't like this because letrec is not
| fundemental to the system but is being forced into the center stage. Perhaps
| enforcing the use of letrec only on functions is enough to eliminate this pass
|
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../language.rkt")

;; Only the pass is provided by this module
(provide label-lambdas)

(: label-lambdas (Cast3-Lang Config . -> . Cast4-Lang))
(define (label-lambdas prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let-values ([(exp count) (run-state (ll-expr exp) count)])
      (Prog (list name count type) exp))))

(: ll-expr (-> C3-Expr (State Nat C4-Expr)))
(define (ll-expr exp)
  (match exp
    ;; The tiny core
    ;; This line should only be reached if the lambda
    ;; is not being bound by a let or a letrec
    [(Lambda f* (Castable ctr e))
     (do (bind-state : (State Nat C4-Expr))
         (id : Uid     <- (uid-state "annon"))
         (e  : C4-Expr <- (ll-expr e))
         (return-state
          (Letrec (list (cons id (Lambda f* (Castable ctr e))))
	   (Var id))))]
    [(Letrec b* e) (ll-let b* e)]
    [(Let b* e) (ll-let b* e)]
    [(If tst csq alt)
     (do (bind-state : (State Nat C4-Expr))
         (tst : C4-Expr <- (ll-expr tst))
         (csq : C4-Expr <- (ll-expr csq))
         (alt : C4-Expr <- (ll-expr alt))
         (return-state (If tst csq alt)))]
    [(App e e*) ;; TODO implement direct call optimization
     (do (bind-state : (State Nat C4-Expr))
         (e  : C4-Expr  <- (ll-expr  e))
         (e* : C4-Expr* <- (ll-expr* e*))
         (return-state (App e e*)))]
    [(Op p e*)
     (do (bind-state : (State Nat C4-Expr))
         (e* : C4-Expr* <- (ll-expr* e*))
         (return-state (Op p e*)))]
    [(Var i)   (return-state (Var i))]
    [(Quote k) (return-state (Quote k))]
    [(Tag t)   (return-state (Tag t))]
    ;; Observables Representation
    [(Blame e)
     (do (bind-state : (State Nat C4-Expr))
         (e  : C4-Expr  <- (ll-expr  e))
         (return-state (Blame e)))]
    [(Observe e t)
     (do (bind-state : (State Nat C4-Expr))
         (e  : C4-Expr  <- (ll-expr  e))
         (return-state (Observe e t)))]
    ;; Type Representation
    [(Type t) (return-state (Type t))]
    [(Type-tag e) (lift-state (inst Type-tag C4-Expr) (ll-expr e))]
    [(Type-Fn-arg e i)
     (do (bind-state : (State Nat C4-Expr))
         (e  : C4-Expr <- (ll-expr  e))
         (i  : C4-Expr <- (ll-expr i))
         (return-state (Type-Fn-arg e i)))]
    [(Type-Fn-return e)
     (do (bind-state : (State Nat C4-Expr))
         (e  : C4-Expr <- (ll-expr  e))
         (return-state (Type-Fn-return e)))]
    [(Type-Fn-arity e)
     (do (bind-state : (State Nat C4-Expr))
         (e  : C4-Expr <- (ll-expr  e))
         (return-state (Type-Fn-arity e)))]
    [(Type-GRef-to e)
     (do (bind-state : (State Nat C4-Expr))
         (e  : C4-Expr <- (ll-expr  e))
         (return-state (Type-GRef-to e)))]
    ;; Dynamic Representation
    [(Dyn-tag e)
     (do (bind-state : (State Nat C4-Expr))
         (e : C4-Expr <- (ll-expr e))
         (return-state (Dyn-tag e)))]
    [(Dyn-immediate e)
     (do (bind-state : (State Nat C4-Expr))
         (e : C4-Expr <- (ll-expr e))
         (return-state (Dyn-immediate e)))]
    [(Dyn-type e)
     (do (bind-state : (State Nat C4-Expr))
         (e : C4-Expr <- (ll-expr e))
         (return-state (Dyn-type e)))]
    [(Dyn-value e)
     (do (bind-state : (State Nat C4-Expr))
         (e : C4-Expr <- (ll-expr e))
         (return-state (Dyn-value e)))]
    [(Dyn-make e1 e2)
     (do (bind-state : (State Nat C4-Expr))
         (e1 : C4-Expr <- (ll-expr e1))
         (e2 : C4-Expr <- (ll-expr e2))
         (return-state (Dyn-make e1 e2)))]
    ;; Function Representation Primitives
    [(Fn-Caster e)
     (do (bind-state : (State Nat C4-Expr))
         (e  : C4-Expr <- (ll-expr  e))
         (return-state (Fn-Caster e)))]
    ;; control flow for effects
    [(Begin e* e)
     (do (bind-state : (State Nat C4-Expr))
         (e* : C4-Expr* <- (ll-expr* e*))
         (e  : C4-Expr  <- (ll-expr  e))
         (return-state (Begin e* e)))]
    [(Repeat i e1 e2 e3)
     (do (bind-state : (State Nat C4-Expr))
         (e1 : C4-Expr <- (ll-expr e1))
         (e2 : C4-Expr <- (ll-expr e2))
         (e3 : C4-Expr <- (ll-expr e3))
         (return-state (Repeat i e1 e2 e3)))]
    ;; Gaurded Representation
    [(GRep-proxied? e)
     (lift-state (inst GRep-proxied? C4-Expr) (ll-expr e))]
    [(UGbox e)
     (lift-state (inst UGbox C4-Expr) (ll-expr e))]
    [(UGbox-ref e)
     (lift-state (inst UGbox-ref C4-Expr) (ll-expr e))]
    [(UGbox-set! e1 e2)
     (do (bind-state : (State Nat C4-Expr))
         (e1 : C4-Expr <- (ll-expr e1))
         (e2 : C4-Expr <- (ll-expr e2))
         (return-state (UGbox-set! e1 e2)))]
    [(Gproxy e1 e2 e3 e4)
     (do (bind-state : (State Nat C4-Expr))
         (e1 : C4-Expr <- (ll-expr e1))
         (e2 : C4-Expr <- (ll-expr e2))
         (e3 : C4-Expr <- (ll-expr e3))
         (e4 : C4-Expr <- (ll-expr e4))
         (return-state (Gproxy e1 e2 e3 e4)))]
    [(Gproxy-for e)
     (lift-state (inst Gproxy-for C4-Expr) (ll-expr e))]
    [(Gproxy-from e)
     (lift-state (inst Gproxy-from C4-Expr) (ll-expr e))]
    [(Gproxy-to e)
     (lift-state (inst Gproxy-to C4-Expr) (ll-expr e))]
    [(Gproxy-blames e)
     (lift-state (inst Gproxy-blames C4-Expr) (ll-expr e))]))

(: ll-expr* (-> C3-Expr* (State Nat C4-Expr*)))
(define (ll-expr* e*) (map-state ll-expr e*))

;; ll-let takes the fields of from let and letrecs and pulls all
;; bound procedures out into a letrec form. Placing
;; the rest as bindings in a let as the body of the let-proc
;; every time I look at this code I realize that one of the oddities
;; is that in a lexical world this would break horribly but invariants
;; about variable renaming prevents this from occuing.
;; We should do some closure and letrec optimization soon!
(: ll-let (-> C3-Bnd* C3-Expr (State Nat C4-Expr)))
(define (ll-let b* e)
  ;; split-bound-procedures actually performs the filtering
  ;; breaking the macro here for now until I have a better operator
  ;; such as fold-state
  (: split-bnds (-> C3-Bnd* Nat
		    (values C4-Bnd-Lambda* C4-Bnd-Data* Nat)))
  (define (split-bnds b* n)
    (for/fold ([bp* : C4-Bnd-Lambda* '()]
	       [bd* : C4-Bnd-Data* '()]
	       [n   : Nat n])
	([b : C3-Bnd b*])
      (match b
	[(cons i (Lambda f* (Castable b e)))
         (let-values ([(e n) (run-state (ll-expr e) n)])
           (let-values ([([bp : C4-Bnd-Lambda]) (cons i (Lambda f* (Castable b e)))])
             (values (cons bp bp*) bd* n)))]
	[(cons i e)
	 (let-values ([(e n) (run-state (ll-expr e) n)])
           (let-values ([([bd : C4-Bnd-Data]) (cons i e)])
             (values bp* (cons bd bd*) n)))])))
  (do (bind-state : (State Nat C4-Expr))
      (n : Nat <- get-state)
      (let-values ([(bp* bd* n) (split-bnds b* n)])
        ((put-state n) : (State Nat Null))
        (e : C4-Expr <- (ll-expr e))
        ;; if either are null then we may remove an empty let
        ;; This transformation is only valid because bindings
        ;; being unique otherwise we might be changing
        ;; the scope on the rhs
        (cond
          [(and (null? bp*) (null? bd*)) (return-state e)]
          [(null? bp*) (return-state (Let bd* e))]
          [(null? bd*) (return-state (Letrec bp* e))]
          [else (return-state (Let bd* (Letrec bp* e)))]))))
