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
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/cast-or-coerce3.rkt"
         "../language/cast-or-coerce4.rkt")
(provide label-lambdas
         (all-from-out
          "../language/cast-or-coerce3.rkt"
         "../language/cast-or-coerce4.rkt"))

(: label-lambdas (Cast3-Lang Config . -> . Cast4-Lang))
(define (label-lambdas prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let-values ([(exp count) (run-state (ll-expr exp) count)])
      (Prog (list name count type) exp))))

(: ll-expr (-> CoC3-Expr (State Nat CoC4-Expr)))
(define (ll-expr exp)
  (do (bind-state : (State Nat CoC4-Expr))
      (match exp
        ;; The tiny core
        ;; This line should only be reached if the lambda
        ;; is not being bound by a let or a letrec
        [(Lambda f* (Castable ctr e))
         (id : Uid     <- (uid-state "annon"))
         (e  : CoC4-Expr <- (ll-expr e))
         (return-state
          (Letrec (list (cons id (Lambda f* (Castable ctr e))))
                  (Var id)))]
        [(Letrec b* e) (ll-let b* e)]
        [(Let b* e) (ll-let b* e)]
        ;; And all the boring cases
        [(If tst csq alt)
                      (tst : CoC4-Expr <- (ll-expr tst))
           (csq : CoC4-Expr <- (ll-expr csq))
           (alt : CoC4-Expr <- (ll-expr alt))
           (return-state (If tst csq alt))]
        [(App e e*) ;; TODO implement direct call optimization
                      (e  : CoC4-Expr  <- (ll-expr  e))
           (e* : CoC4-Expr* <- (ll-expr* e*))
           (return-state (App e e*))]
        [(Op p e*)
         (e* : CoC4-Expr* <- (ll-expr* e*))
         (return-state (Op p e*))]
        [(Var i)   (return-state (Var i))]
        [(Quote k) (return-state (Quote k))]
        [(Tag t)   (return-state (Tag t))]
        ;; Observables Representation
        [(Blame e)
         (e  : CoC4-Expr  <- (ll-expr  e))
         (return-state (Blame e))]
        [(Observe e t)
         (e  : CoC4-Expr  <- (ll-expr  e))
         (return-state (Observe e t))]
        ;; Type Representation
        [(Type t) (return-state (Type t))]
        [(Type-tag e)
         (e : CoC4-Expr <- (ll-expr e))
         (return-state (Type-tag e))]
        [(Type-Fn-arg e i)
                      (e  : CoC4-Expr <- (ll-expr  e))
           (i  : CoC4-Expr <- (ll-expr i))
           (return-state (Type-Fn-arg e i))]
        [(Type-Fn-return e)
                      (e  : CoC4-Expr <- (ll-expr  e))
           (return-state (Type-Fn-return e))]
        [(Type-Fn-arity e)
             (e  : CoC4-Expr <- (ll-expr  e))
           (return-state (Type-Fn-arity e))]
        [(Type-GRef-to e)
             (e  : CoC4-Expr <- (ll-expr e))
           (return-state (Type-GRef-to e))]
        [(Type-GVect-to e)
             (e : CoC4-Expr <- (ll-expr e))
           (return-state (Type-GVect-to e))
]
        ;; Dynamic Representation
        [(Dyn-tag e)
             (e : CoC4-Expr <- (ll-expr e))
           (return-state (Dyn-tag e))]
        [(Dyn-immediate e)
             (e : CoC4-Expr <- (ll-expr e))
           (return-state (Dyn-immediate e))]
        [(Dyn-type e)
             (e : CoC4-Expr <- (ll-expr e))
           (return-state (Dyn-type e))]
        [(Dyn-value e)
             (e : CoC4-Expr <- (ll-expr e))
           (return-state (Dyn-value e))]
        [(Dyn-make e1 e2)
             (e1 : CoC4-Expr <- (ll-expr e1))
           (e2 : CoC4-Expr <- (ll-expr e2))
           (return-state (Dyn-make e1 e2))]
        ;; Function Representation Primitives
        [(Fn-Caster e)
             (e  : CoC4-Expr <- (ll-expr  e))
           (return-state (Fn-Caster e))]
        ;; control flow for effects
        [(Begin e* e)
             (e* : CoC4-Expr* <- (ll-expr* e*))
           (e  : CoC4-Expr  <- (ll-expr  e))
           (return-state (Begin e* e))]
        [(Repeat i e1 e2 e3)
                      (e1 : CoC4-Expr <- (ll-expr e1))
           (e2 : CoC4-Expr <- (ll-expr e2))
           (e3 : CoC4-Expr <- (ll-expr e3))
           (return-state (Repeat i e1 e2 e3))]
        ;; Gaurded Representation
        [(Guarded-Proxy-Huh e)
         (e : CoC4-Expr <- (ll-expr e))
         (return-state (Guarded-Proxy-Huh e))]
        [(Unguarded-Box e)
         (e : CoC4-Expr <- (ll-expr e))
         (return-state (Unuarded-Box e))]
        [(Unguarde-Box-Ref e)
         (e : CoC4-Expr <- (ll-expr e))
         (return-state (Unuarded-Box-Ref e))]
        [(UGbox-set! e1 e2)
         (do (bind-state : (State Nat CoC4-Expr))
             (e1 : CoC4-Expr <- (ll-expr e1))
           (e2 : CoC4-Expr <- (ll-expr e2))
           (return-state (UGbox-set! e1 e2)))]
        [(UGvect e1 e2)
         (lift-state (inst UGvect CoC4-Expr CoC4-Expr) (ll-expr e1) (ll-expr e2))]
        [(UGvect-ref e1 e2)
         (lift-state (inst UGvect-ref CoC4-Expr CoC4-Expr) (ll-expr e1) (ll-expr e2))]
        [(UGvect-set! e1 e2 e3)
         (do (bind-state : (State Nat CoC4-Expr))
             (e1 : CoC4-Expr <- (ll-expr e1))
           (e2 : CoC4-Expr <- (ll-expr e2))
           (e3 : CoC4-Expr <- (ll-expr e3))
           (return-state (UGvect-set! e1 e2 e3)))]
        [(Gproxy e1 e2 e3 e4)
         (do (bind-state : (State Nat CoC4-Expr))
             (e1 : CoC4-Expr <- (ll-expr e1))
           (e2 : CoC4-Expr <- (ll-expr e2))
           (e3 : CoC4-Expr <- (ll-expr e3))
           (e4 : CoC4-Expr <- (ll-expr e4))
           (return-state (Gproxy e1 e2 e3 e4)))]
        [(Gproxy-for e)
         (lift-state (inst Gproxy-for CoC4-Expr) (ll-expr e))]
        [(Gproxy-from e)
         (lift-state (inst Gproxy-from CoC4-Expr) (ll-expr e))]
        [(Gproxy-to e)
         (lift-state (inst Gproxy-to CoC4-Expr) (ll-expr e))]
        [(Gproxy-blames e)
         (lift-state (inst Gproxy-blames CoC4-Expr) (ll-expr e))])
    ))

(: ll-expr* (-> CoC3-Expr* (State Nat CoC4-Expr*)))
(define (ll-expr* e*) (map-state ll-expr e*))


;; ll-let takes the fields of from let and letrecs and pulls all
;; bound procedures out into a letrec form. Placing
;; the rest as bindings in a let as the body of the let-proc
;; every time I look at this code I realize that one of the oddities
;; is that in a lexical world this would break horribly but invariants
;; about variable renaming prevents this from occuing.
;; We should do some closure and letrec optimization soon!
(: ll-let (-> CoC3-Bnd* CoC3-Expr (State Nat CoC4-Expr)))
(define (ll-let b* e)
  ;; split-bound-procedures actually performs the filtering
  ;; breaking the macro here for now until I have a better operator
  ;; such as fold-state
  (: split-bnds (-> CoC3-Bnd* Nat
		    (values CoC4-Bnd-Lambda* CoC4-Bnd-Data* Nat)))
  (define (split-bnds b* n)
    (for/fold ([bp* : CoC4-Bnd-Lambda* '()]
	       [bd* : CoC4-Bnd-Data* '()]
	       [n   : Nat n])
	([b : CoC3-Bnd b*])
      (match b
	[(cons i (Lambda f* (Castable b e)))
         (let-values ([(e n) (run-state (ll-expr e) n)])
           (let-values ([([bp : CoC4-Bnd-Lambda]) (cons i (Lambda f* (Castable b e)))])
             (values (cons bp bp*) bd* n)))]
	[(cons i e)
	 (let-values ([(e n) (run-state (ll-expr e) n)])
           (let-values ([([bd : CoC4-Bnd-Data]) (cons i e)])
             (values bp* (cons bd bd*) n)))])))
  (do (bind-state : (State Nat CoC4-Expr))
      (n : Nat <- get-state)
      (let-values ([(bp* bd* n) (split-bnds b* n)])
        ((put-state n) : (State Nat Null))
        (e : CoC4-Expr <- (ll-expr e))
        ;; if either are null then we may remove an empty let
        ;; This transformation is only valid because bindings
        ;; being unique otherwise we might be changing
        ;; the scope on the rhs
        (cond
          [(and (null? bp*) (null? bd*)) (return-state e)]
          [(null? bp*) (return-state (Let bd* e))]
          [(null? bd*) (return-state (Letrec bp* e))]
          [else (return-state (Let bd* (Letrec bp* e)))]))))
