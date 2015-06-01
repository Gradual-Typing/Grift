#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: compiler/casts/uncover-free                                              |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass labels all lambda with there free variables by adding
| the captures language form. This will soon disappear as it is replaced with
| non lexical lambdas
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require schml/src/language
         schml/src/helpers
         schml/src/errors)

;; Only the pass is provided by this module
(provide uncover-free)

(: uncover-free (Cast4-Lang Config . -> . Cast5-Lang))
(define (uncover-free prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let-values ([(exp free*) (uf-expr exp)])
      (if (set-empty? free*)
	  (Prog (list name count type) exp)
	  (raise-pass-exn 'uncover-free "Free variables detect ~a" free*)))))

(: uf-expr (-> C4-Expr (Values C5-Expr (Setof Uid))))
(define (uf-expr e)
  (match e
    ;; Core
    [(Letrec (app uf-bnd-lambda* b*-bvars b* b*-fvars)
             (app uf-expr e e-fvars))
     (values (Letrec b* e) (set-subtract (set-union e-fvars b*-fvars) b*-bvars))]
    [(Let (app uf-bnd-data* b*-bvars b* b*-fvars)
          (app uf-expr e e-fvars))
     (values (Let b* e) (set-subtract (set-union e-fvars b*-fvars) b*-bvars))]
    [(If (app uf-expr t t-fvars)
         (app uf-expr c c-fvars)
         (app uf-expr a a-fvars))
     (values (If t c a) (set-union t-fvars c-fvars a-fvars))]
    [(App (app uf-expr e e-fvars) (app uf-expr* e* e*-fvars))
     (values (App e e*) (set-union e-fvars e*-fvars))]
    [(Op p (app uf-expr* e* e*-fvars)) (values (Op p e*) e*-fvars)]
    [(Var u) (values (Var u) (set u))]
    [(Quote k) (values (Quote k) (set))]
    [(Tag t) (values (Tag t) (set))]
    ;; Observables Representation
    [(Blame (app uf-expr e f)) (values (Blame e) f)]
    [(Observe (app uf-expr e f) t) (values (Observe e t) f)]
    ;; Type Representation
    [(Type t) (values (Type t) (set))]
    [(Type-tag (app uf-expr e e-fvars)) (values (Type-tag e) e-fvars)]
    [(Type-GRef-to (app uf-expr e f*)) (values (Type-GRef-to e) f*)]
    [(Type-Fn-arg (app uf-expr e e-fvars) (app uf-expr i i-fvars))
     (values (Type-Fn-arg e i) (set-union e-fvars i-fvars))]
    [(Type-Fn-return (app uf-expr e e-fvars))
     (values (Type-Fn-return e) e-fvars)]
    [(Type-Fn-arity (app uf-expr e e-fvars))
     (values (Type-Fn-arity e) e-fvars)]
    ;; Dynamic Representation
    [(Dyn-tag (app uf-expr e e-fvars)) (values (Dyn-tag e) e-fvars)]
    [(Dyn-immediate (app uf-expr e e-fvars)) (values (Dyn-immediate e) e-fvars)]
    [(Dyn-type (app uf-expr e e-fvars)) (values (Dyn-type e) e-fvars)]
    [(Dyn-value (app uf-expr e e-fvars)) (values (Dyn-value e) e-fvars)]
    [(Dyn-make (app uf-expr e1 e1-fvars)
               (app uf-expr e2 e2-fvars))
     (values (Dyn-make e1 e2) (set-union e1-fvars e2-fvars))]
    ;; Function Representation Primitives
    [(Fn-Caster (app uf-expr e e-fvars)) (values (Fn-Caster e) e-fvars)]
    ;; control flow for effects
    [(Begin (app uf-expr* e* e*-fvars)
            (app uf-expr  e  e-fvars))
     (values (Begin e* e) (set-union e*-fvars e-fvars))]
    [(Repeat i (app uf-expr e1 f1) (app uf-expr e2 f2) (app uf-expr e3 f3))
     (values (Repeat i e1 e2 e3) (set-subtract (set-union f1 f2 f3) (set i)))]
    ;; Gaurded Representation
    [(GRep-proxied? (app uf-expr e fvars)) (values (GRep-proxied? e) fvars)]
    [(UGbox (app uf-expr e fvars)) (values (UGbox e) fvars)]
    [(UGbox-ref (app uf-expr e fvars)) (values (UGbox-ref e) fvars)]
    [(UGbox-set! (app uf-expr e1 e1-fvars)
                 (app uf-expr e2 e2-fvars))
     (values (UGbox-set! e1 e2) (set-union e1-fvars e2-fvars))]
    [(Gproxy (app uf-expr e1 e1-fvars)
             (app uf-expr e2 e2-fvars)
             (app uf-expr e3 e3-fvars)
             (app uf-expr e4 e4-fvars))
     (values (Gproxy e1 e2 e3 e4)
             (set-union e1-fvars e2-fvars e3-fvars e4-fvars))]
    [(Gproxy-for (app uf-expr e fvars)) (values (Gproxy-for e) fvars)]
    [(Gproxy-from (app uf-expr e fvars)) (values (Gproxy-from e) fvars)]
    [(Gproxy-to   (app uf-expr e fvars)) (values (Gproxy-to e) fvars)]
    [(Gproxy-blames (app uf-expr e fvars)) (values (Gproxy-blames e) fvars)]))

;; Begin terribleness that comes from using values they do not compose consider
;; getting rid of them and just using cons cells
;; in general though it is hard to get polymorphic types to compose in typed
;; racket so ...

(: uf-expr* (-> (Listof C4-Expr)
		(values (Listof C5-Expr) (Setof Uid))))
(define (uf-expr* e*)
  (if (null? e*)
      (values '() (set))
      (let ([a (car e*)]
	    [d (cdr e*)])
	(let-values ([(e* e*-fvars) (uf-expr* d)]
		     [(e e-fvars) (uf-expr a)])
	  (values (cons e e*) (set-union e*-fvars e-fvars))))))

(: uf-lambda (C4-Lambda . -> . (values C5-Lambda (Setof Uid))))
(define (uf-lambda lam)
  (: id-subtract (-> Uid (Setof Uid) (Setof Uid)))
  ;; id-subtract is set remove with the arguments in reverse
  (define (id-subtract f s) (set-remove s f))
  (match-let ([(Lambda f* (Castable ctr? (app uf-expr e fvars))) lam])
    #;(TODO the second call to fvars is bloating closures and is could likely be removed)
    (let* ([fvars  (foldl id-subtract fvars f*)]
           [fvars^ (if ctr? (set-add fvars ctr?) fvars)])
      (values (Lambda f* (Free ctr? (set->list fvars) e)) fvars^))))

(: uf-bnd-lambda* (-> C4-Bnd-Lambda*
                      (values (Setof Uid)
                              (Listof (Pairof Uid C5-Lambda))
                              (Setof Uid))))
(define (uf-bnd-lambda* b*)
  (if (null? b*)
      (values (set) '() (set))
      (let ([a (car b*)] [d (cdr b*)]) ;; free the list
        (let-values ([(u* b* f*) (uf-bnd-lambda* d)])
          (match-let ([(cons u (app uf-lambda rhs rhs-f*)) a])
            (values (set-add u* u)
                    (cons (cons u rhs) b*)
                    (set-union f* rhs-f*)))))))

(: uf-bnd-data* (-> C4-Bnd-Data*
                    (values (Setof Uid)
                            (Listof (Pairof Uid C5-Expr))
                            (Setof Uid))))
(define (uf-bnd-data* b*)
  (if (null? b*)
      (values (set) '() (set))
      (let ([a (car b*)] [d (cdr b*)]) ;; free the list
        (let-values ([(u* b* f*) (uf-bnd-data* d)])
          (match-let ([(cons u (app uf-expr rhs rhs-f*)) a])
            (values (set-add u* u)
                    (cons (cons u rhs) b*)
                    (set-union f* rhs-f*)))))))
