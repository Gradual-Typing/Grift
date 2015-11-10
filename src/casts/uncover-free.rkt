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
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/cast-or-coerce4.rkt"
         "../language/cast-or-coerce5.rkt")

;; Only the pass is provided by this module
(provide uncover-free)

(: uncover-free (Cast-or-Coerce4-Lang Config . -> . Cast-or-Coerce5-Lang))
(define (uncover-free prgm comp-config)
  (match-let ([(Prog (list name count type) exp) prgm])
    (let-values ([(exp free*) (uf-expr exp)])
      (if (set-empty? free*)
	  (Prog (list name count type) exp)
	  (raise-pass-exn 'uncover-free "Free variables detect ~a" free*)))))

(: uf-expr (CoC4-Expr -> (values CoC5-Expr (Setof Uid))))
(define (uf-expr e)
  (match e
    ;; Interesting Cases
    ;; Free variables of an expression are returned in the set
    [(Var u) (values (Var u) (set u))]
    ;; All binding forms must both filter variables bound at
    ;; this site and return the free variables of the expression
    [(Letrec (app uf-bnd-lambda* b*-bvars b* b*-fvars)
             (app uf-expr e e-fvars))
     (values (Letrec b* e) (set-subtract (set-union e-fvars b*-fvars) b*-bvars))]
    [(Let (app uf-bnd-data* b*-bvars b* b*-fvars)
          (app uf-expr e e-fvars))
     (values (Let b* e) (set-subtract (set-union e-fvars b*-fvars) b*-bvars))]
    ;; I do not think that labels need to be treated like variables
    [(Labels (app uf-bnd-code* b*) (app uf-expr e fv))
     (values (Labels b* e) fv)]
    [(Repeat i (app uf-expr e1 f1) (app uf-expr e2 f2) (app uf-expr e3 f3))
     (values (Repeat i e1 e2 e3) (set-subtract (set-union f1 f2 f3) (set i)))]
    
    [(If (app uf-expr t t-fv) (app uf-expr c c-fv) (app uf-expr a a-fv))
     (values (If t c a) (set-union t-fv c-fv a-fv))]
    [(Op p (app uf-expr* e* e*-fvars)) (values (Op p e*) e*-fvars)]
    [(Quote k) (values (Quote k) (set))]
    [(Tag t) (values (Tag t) (set))]
    ;; Observables Representation
    [(Blame (app uf-expr e f)) (values (Blame e) f)]
    [(Observe (app uf-expr e f) t) (values (Observe e t) f)]
    ;; Dynamic Representation
    [(Dyn-tag (app uf-expr e fv))
     (values (Dyn-tag e) fv)]
    [(Dyn-immediate (app uf-expr e e-fv))
     (values (Dyn-immediate e) e-fv)]
    [(Dyn-type (app uf-expr e e-fv))
     (values (Dyn-type e) e-fv)]
    [(Dyn-value (app uf-expr e e-fvars)) (values (Dyn-value e) e-fvars)]
    [(Dyn-make (app uf-expr e1 e1-fvars)
               (app uf-expr e2 e2-fvars))
     (values (Dyn-make e1 e2) (set-union e1-fvars e2-fvars))]
    ;; Function Representation Primitives
    [(Fn-Caster (app uf-expr e e-fvars)) (values (Fn-Caster e) e-fvars)]
    ;; control flow for effects
    [(Begin (app uf-expr* e* fv1) (app uf-expr e fv2))
     (values (Begin e* e) (set-union fv1 fv2))]
    ;; Type Representation
    [(Type t) (values (Type t) (set))]
    [(Type-Tag (app uf-expr e fv))
     (values (Type-Tag e) fv)]
    [(Type-GRef-Of (app uf-expr e f*))
     (values (Type-GRef-Of e) f*)]
    [(Type-GVect-Of (app uf-expr e f*))
     (values (Type-GVect-Of e) f*)]
    [(Type-Fn-arg (app uf-expr e e-fvars) (app uf-expr i i-fvars))
     (values (Type-Fn-arg e i) (set-union e-fvars i-fvars))]
    [(Type-Fn-return (app uf-expr e e-fvars))
     (values (Type-Fn-return e) e-fvars)]
    [(Type-Fn-arity (app uf-expr e e-fvars))
     (values (Type-Fn-arity e) e-fvars)]
    ;; Begin new stuff TODO take out this comment once the code works
    [(Type-Dyn-Huh (app uf-expr e fv)) (values (Type-Dyn-Huh e) fv)]
    [(Type-Fn-Huh (app uf-expr e fv)) (values (Type-Fn-Huh e) fv)]
    [(Type-GRef-Huh (app uf-expr e fv)) (values (Type-GRef-Huh e) fv)]
    [(Type-GVect-Huh (app uf-expr e fv)) (values (Type-GVect-Huh e) fv)]
    ;; Code Representation
    [(Code-Label u) (values (Code-Label u) (set))]
    [(App-Code (app uf-expr e e-fv) (app uf-expr* e* e*-fv))
     (values (App-Code e e*) (set-union e-fv e*-fv))]
    [(App-Fn (app uf-expr e e-fv) (app uf-expr* e* e*-fv))
     (values (App-Fn e e*) (set-union e-fv e*-fv))]
    [(App-Fn-or-Proxy i (app uf-expr e e-fv) (app uf-expr* e* e*-fv))
     (values (App-Fn-or-Proxy i e e*) (set-union e-fv e*-fv))]
    ;; Coercion Representation Stuff
    [(Quote-Coercion c)
     (values (Quote-Coercion c) (set))]
    [(Id-Coercion-Huh (app uf-expr e fv))
     (values (Id-Coercion-Huh e) fv)]
    [(Fn-Coercion-Huh (app uf-expr e fv))
     (values (Fn-Coercion-Huh e) fv)]
    [(Make-Fn-Coercion
      u (app uf-expr e1 fv1) (app uf-expr e2 fv2) (app uf-expr e3 fv3))
     (values (Make-Fn-Coercion u e1 e2 e3)
             (set-union fv1 fv2 fv3))]
    [(Compose-Fn-Coercion u (app uf-expr e1 fv1) (app uf-expr e2 fv2))
     (values (Compose-Fn-Coercion u e1 e2)
             (set-union fv1 fv2))]
    [(Fn-Coercion (app uf-expr* e* fv1) (app uf-expr e fv2))
     (values (Fn-Coercion e* e) (set-union fv1 fv2))]
    [(Fn-Coercion-Arg (app uf-expr e1 fv1)(app uf-expr e2 fv2))
     (values (Fn-Coercion-Arg e1 e2) (set-union fv1 fv2))]
    [(Fn-Coercion-Return (app uf-expr e fv))
     (values (Fn-Coercion-Return e) fv)]
    [(Ref-Coercion (app uf-expr e1 fv1) (app uf-expr e2 fv2))
     (values (Ref-Coercion e1 e2) (set-union fv1 fv2))]
    [(Ref-Coercion-Huh (app uf-expr e fv))
     (values (Ref-Coercion-Huh e) fv)]
    [(Ref-Coercion-Read (app uf-expr e fv))
     (values (Ref-Coercion-Read e) fv)]
    [(Ref-Coercion-Write (app uf-expr e fv))
     (values (Ref-Coercion-Write e) fv)]
    [(Sequence-Coercion (app uf-expr e1 fv1) (app uf-expr e2 fv2))
     (values (Sequence-Coercion e1 e2) (set-union fv1 fv2))]
    [(Sequence-Coercion-Huh (app uf-expr e fv))
     (values (Sequence-Coercion-Huh e) fv)]
    [(Sequence-Coercion-Fst (app uf-expr e fv))
     (values (Sequence-Coercion-Fst e) fv)]
    [(Sequence-Coercion-Snd (app uf-expr e fv))
     (values (Sequence-Coercion-Snd e) fv)]
    [(Project-Coercion (app uf-expr e1 fv1) (app uf-expr e2 fv2))
     (values (Project-Coercion e1 e2) (set-union fv1 fv2))]
    [(Project-Coercion-Huh (app uf-expr e fv))
     (values (Project-Coercion-Huh e) fv)]
    [(Project-Coercion-Type (app uf-expr e fv))
     (values (Project-Coercion-Type e) fv)]
    [(Project-Coercion-Label (app uf-expr e fv))
     (values (Project-Coercion-Label e) fv)]
    [(Inject-Coercion (app uf-expr e fv))
     (values (Inject-Coercion e) fv)]
    [(Inject-Coercion-Type (app uf-expr e fv))
     (values (Inject-Coercion-Type e) fv)]
    [(Inject-Coercion-Huh (app uf-expr e fv))
     (values (Inject-Coercion-Huh e) fv)]
    [(Failed-Coercion (app uf-expr e fv))
     (values (Failed-Coercion e) fv)]
    [(Failed-Coercion-Huh (app uf-expr e fv))
     (values (Failed-Coercion-Huh e) fv)]
    [(Failed-Coercion-Label (app uf-expr e fv))
     (values (Failed-Coercion-Label e) fv)]
    ;; Function Proxy Representation
    [(Fn-Proxy i (app uf-expr e1 fv1)(app uf-expr e2 fv2))
     (values (Fn-Proxy i e1 e2) (set-union fv1 fv2))]
    [(Fn-Proxy-Huh (app uf-expr e fv))
     (values (Fn-Proxy-Huh e) fv)]
    [(Fn-Proxy-Closure (app uf-expr e fv))
     (values (Fn-Proxy-Closure e) fv)]
    [(Fn-Proxy-Coercion (app uf-expr e fv))
     (values (Fn-Proxy-Coercion e) fv)]
    ;; Gaurded Representation
    [(Unguarded-Box (app uf-expr e fv))
     (values (Unguarded-Box e) fv)]
    [(Unguarded-Box-Ref (app uf-expr e fv))
     (values (Unguarded-Box-Ref e) fv)]
    [(Unguarded-Box-Set! (app uf-expr e1 fv1) (app uf-expr e2 fv2))
     (values (Unguarded-Box-Set! e1 e2) (set-union fv1 fv2))]
    [(Unguarded-Vect (app uf-expr e1 fv1) (app uf-expr e2 fv2))
     (values (Unguarded-Vect e1 e2) (set-union fv1 fv2))]
    [(Unguarded-Vect-Ref (app uf-expr e1 fv1) (app uf-expr e2 fv2))
     (values (Unguarded-Vect-Ref e1 e2) (set-union fv1 fv2))]
    [(Unguarded-Vect-Set!
      (app uf-expr e1 fv1) (app uf-expr e2 fv2) (app uf-expr e3 fv3))
     (values (Unguarded-Vect-Set! e1 e2 e3) (set-union fv1 fv2 fv3))]
    [(Guarded-Proxy-Huh (app uf-expr e fv))
     (values (Guarded-Proxy-Huh e) fv)]
    [(Guarded-Proxy (app uf-expr e fv0) r)
     (match r
       [(Twosome (app uf-expr t1 fv1) (app uf-expr t2 fv2) (app uf-expr l fv3))
        (values (Guarded-Proxy e (Twosome t1 t2 l))
                (set-union fv0 fv1 fv2 fv3))]
       [(Coercion (app uf-expr c fv1))
        (values (Guarded-Proxy e (Coercion c)) (set-union fv0 fv1))])]
    [(Guarded-Proxy-Ref (app uf-expr e fv))
     (values (Guarded-Proxy-Ref e) fv)]
    [(Guarded-Proxy-Source (app uf-expr e fv))
     (values (Guarded-Proxy-Source e) fv)]
    [(Guarded-Proxy-Target (app uf-expr e fv))
     (values (Guarded-Proxy-Target e) fv)]
    [(Guarded-Proxy-Blames (app uf-expr e fv))
     (values (Guarded-Proxy-Blames e) fv)]
    [(Guarded-Proxy-Coercion (app uf-expr e fv))
     (values (Guarded-Proxy-Coercion e) fv)]
    [other (error 'uncover-free "unmatched ~a" other)]))


(: uf-expr* (-> (Listof CoC4-Expr) (Values (Listof CoC5-Expr) (Setof Uid))))
(define (uf-expr* e*)
  (: help (CoC4-Expr (Pair CoC5-Expr* (Setof Uid)) -> (Pair CoC5-Expr* (Setof Uid))))
  (define (help e a)
    (let-values ([(e  fv1) (uf-expr e)]
                 [(e* fv2) (values (car a) (cdr a))])
      (cons (cons e e*) (set-union fv1 fv2))))
  (define acc (cons '() (ann (set) (Setof Uid))))
  (define e.fv (foldr help acc e*))
  (values (car e.fv) (cdr e.fv)))

#|
(: uf-lambda (CoC4-Lambda . -> . (Values CoC5-Lambda (Setof Uid))))
(define (uf-lambda lam)
  (match-let ([(Lambda u* (Castable ctr? (app uf-expr e fvars))) lam])
    (let* ([fvars  (set-subtract fv (list->set u*))]
           [fvars^ (if ctr? (set-add fvars ctr?) fvars)])
      (values (Lambda f* (Free ctr? (set->list fvars) e)) fvars^))))
|#

(: uf-bnd-lambda*
   (-> CoC4-Bnd-Lambda*
       (values (Setof Uid)
               (Listof (Pairof Uid CoC5-Lambda))
               (Setof Uid))))
(define (uf-bnd-lambda* b*)
  (for/fold ([bv   : (Setof Uid)      (set)]
             [bnd* : CoC5-Bnd-Lambda*   '()]
             [fv   : (Setof Uid)      (set)])
            ([b    : CoC4-Bnd-Lambda     b*])
    (match-let ([(cons u (Lambda u* (Castable ctr? (app uf-expr e e-fv)))) b])
      (let* ([l-fv (set-subtract e-fv (list->set u*))]
             #;(TODO the second call to fvars is bloating closures and is could likely be removed)
             #;[l-fv (if ctr? (set-add l-fv ctr?) l-fv)]
             )
        (values (set-add bv u)
                (cons (cons u (Lambda u* (Free ctr? (set->list l-fv) e)))
                      bnd*)
                (set-union fv l-fv))))))

#|

  (if (null? b*)
      (values (set) '() (set))
      (let ([a (car b*)] [d (cdr b*)]) ;; free the list
        (let-values ([(u* b* f*) (uf-bnd-lambda* d)])
          (match-let ([(cons u (app uf-lambda rhs rhs-f*)) a])
            (values (set-add u* u)
                    (cons (cons u rhs) b*)
                    (set-union f* rhs-f*))))))|#

(: uf-bnd-data*
   (-> CoC4-Bnd-Data*
       (values (Setof Uid) CoC5-Bnd-Data* (Setof Uid))))
(define (uf-bnd-data* b*)
  (for/fold ([bv   : (Setof Uid)    (set)]
             [bnd* : CoC5-Bnd-Data*   '()]
             [fv   : (Setof Uid)    (set)])
            ([b : CoC4-Bnd-Data        b*])
    (match-let ([(cons u (app uf-expr e e-fv)) b])
      (values (set-add bv u)
              (cons (cons u e) bnd*)
              (set-union e-fv fv)))))

(: uf-bnd-code* (CoC4-Bnd-Code* -> CoC5-Bnd-Code*))
(define (uf-bnd-code* b*)
  (for/list : (Listof CoC5-Bnd-Code) ([b : CoC4-Bnd-Code b*])
    (match-let ([(cons u (Code u* (app uf-expr e e-fv))) b])
      (unless (set-empty? (set-subtract e-fv (list->set u*)))
        (error 'uncover-free "Code should never have free variables ~a\n\t~a" b (set-subtract e-fv (list->set u*))))
      (cons u (Code u* e)))))
