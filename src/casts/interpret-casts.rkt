#lang typed/racket/base
#|-----------------------------------------------------------------------------+
|Pass: src/casts/interpret-casts                                          |
+------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                             |
+------------------------------------------------------------------------------+
|Description:
- This pass creates a cast-interpreter that can cast arbitrary values at
runtime.
- Converts runtime casts to applications of the cast-interpreter.
- Specializes casts with known types, at this point represented with the cast
form, to the shortest branch of the cast tree that is relevant.
- Replaces types that are structure with a psuedo constructor call
- Replaces tags atomic types with the Type form.
- Introduces the short lived dynamic type manipulation forms
+------------------------------------------------------------------------------+
|Input Grammar Cast2-Language                                                  |
|Output Grammar Cast3-Language                                                 |
+-----------------------------------------------------------------------------|#
;; The define-pass syntax
(require
 racket/match
 racket/format
 "../helpers.rkt"
 "../errors.rkt"
 "../configuration.rkt"
 "../language/cast-or-coerce2.rkt"
 "../language/cast-or-coerce3.rkt"
 "./interpret-casts/coercions.rkt"
 "./interpret-casts/twosomes.rkt")

(provide
 interpret-casts
 (all-from-out
  "../language/cast-or-coerce2.rkt"
  "../language/cast-or-coerce3.rkt"))

;; Unfortanately this pass delegates most of the work to two other files.
;;;; This occured because typed racket started running out of memory while
;;;; type-checking. Part of the excessive memory consumption is due to
;;;; Typed Racket inturning alot of information on a per file basis.
;;;; Splitting a file therefore causes a dump of this information before
;;;; continuing to typecheck.
;;;; Perhaps we should suggest generational interning.
;; The two files to look for are:
;; interpret-casts/twosomes.rkt
;; interpret-casts/coercions.rkt
;; The common code for these files is stored in:
;; interpret-casts/help.rkt

(: interpret-casts (Cast-or-Coerce2-Lang Config . -> . Cast-or-Coerce3-Lang))
(define (interpret-casts prgm config)
  (define cast-rep (Config-cast-rep config))
  (match-let ([(Prog (list name next type) exp) prgm])
    (define-values (exp^ next^)
      (run-state
       (do (bind-state : (State Nat CoC3-Expr))
           ((list mk-cast mk-compose mk-apply mk-fn-proxy bnd*)
            : (List Cast-Rule ComposeT ApplyT Fn-ProxyT CoC3-Bnd-Code*)
            <- (cond
                 [(eq? 'Twosomes cast-rep) (get-twosome-helpers 'Hybrid)]
                 [(eq? 'Coercions cast-rep) (get-coercion-helpers 'Hybrid)]
                 [else (error 'interpret-casts/cast-rep "~a" cast-rep)]))
            (exp : CoC3-Expr
                 <- ((ic-expr mk-cast mk-compose mk-apply mk-fn-proxy) exp))
            (return-state (Labels bnd* (Observe exp type))))
       next))
    (Prog (list name next^ type) exp^)))


;; Traverse the expression envoking the Cast-Rule where appropriate
(: ic-expr (Cast-Rule ComposeT ApplyT Fn-ProxyT -> IC-ExprT))
(define-type IC-ExprT (CoC2-Expr -> (State Nat CoC3-Expr)))
(define ((ic-expr mk-cast mk-compose mk-apply mk-fn-proxy) exp)
  (: recur IC-ExprT)
  (define (recur exp)
    (do (bind-state : (State Nat CoC3-Expr))
        (match exp
          ;; Interesting Transformations
          [(Interpreted-Cast v r)
           (v  : CoC3-Expr <- (recur v))
           (match r
             [(Twosome t1 t2 l) 
              (t1 : CoC3-Expr <- (recur t1))
              (t2 : CoC3-Expr <- (recur t2))
              (l  : CoC3-Expr <- (recur l))
              (mk-cast v (Twosome t1 t2 l))]
             [(Coercion c)
              (c : CoC3-Expr <- (recur c))
              (mk-cast v (Coercion c))])]
          [(Cast v r)
           (v  : CoC3-Expr <- (recur v))
           (match r
             [(Twosome t1 t2 l)
              (mk-cast v (Twosome (Type t1) (Type t2) (Quote l)))]
             [(Coercion c)
              (mk-cast v (Coercion (Quote-Coercion c)))])]
          [(Compose-Coercions c1 c2)
           (c1 : CoC3-Expr <- (recur c1))
           (c2 : CoC3-Expr <- (recur c2))
           (mk-compose c1 c2)]
          [(Fn-Proxy i e1 e2)
           (e1 : CoC3-Expr <- (recur e1))
           (e2 : CoC3-Expr <- (recur e2))
           (mk-fn-proxy i e1 e2)]
          [(App/Fn-Proxy-Huh e e*)
           (e  : CoC3-Expr  <- (recur e))
           (e* : CoC3-Expr* <- (map-state recur e*))
           (mk-apply e e*)]
          [other (ic-expr-boring-cases other)])))
  ;; This function takes care of the boring cases
  (: ic-expr-boring-cases (-> CoC2-Expr (State Nat CoC3-Expr)))
  (define (ic-expr-boring-cases exp)
    (do (bind-state : (State Nat CoC3-Expr))
        (match exp
          [(Code-Label u) (return-state (Code-Label u))]
          [(Labels c* b)
           (c* : CoC3-Bnd-Code* <- (map-state (ic-bndc recur) c*))
           (e  : CoC3-Expr      <- (recur b))
           (return-state (Labels c* e))]
          [(App-Code e e*)
           (e  : CoC3-Expr  <- (recur e))
           (e* : CoC3-Expr* <- (map-state recur e*))
           (return-state (App-Code e e*))]
          [(Lambda f* (Castable ctr exp))
           (exp : CoC3-Expr <- (recur exp))
           (return-state (Lambda f* (Castable ctr exp)))]
          [(App-Fn e e*)
           (e  : CoC3-Expr  <- (recur e))
           (e* : CoC3-Expr* <- (map-state recur e*))
           (return-state (App-Fn e e*))]
          [(Letrec b* exp)
           (b*  : CoC3-Bnd* <- (map-state (ic-bnd recur) b*))
           (exp : CoC3-Expr <- (recur exp))
           (return-state (Letrec b* exp))]
          [(Let b* exp)
           (b*  : CoC3-Bnd* <- (map-state (ic-bnd recur) b*))
           (exp : CoC3-Expr <- (recur exp))
           (return-state (Let b* exp))]
          [(Op p exp*)
           (exp* : CoC3-Expr* <- (map-state recur exp*))
           (return-state (Op p exp*))]
          [(Fn-Proxy-Huh e)
           (e : CoC3-Expr <- (recur e))
           (return-state (Fn-Proxy-Huh e))]
          [(Fn-Proxy-Closure e)
           (e : CoC3-Expr <- (recur e))
           (return-state (Fn-Proxy-Closure e))]
          [(Fn-Proxy-Coercion e)
           (e : CoC3-Expr <- (recur e))
           (return-state (Fn-Proxy-Coercion e))]
          [(Fn-Caster e)
           (e  : CoC3-Expr <- (recur e))
           (return-state (Fn-Caster e))]
          ;; Coercions manipulation
          [(Id-Coercion-Huh e)
           (e : CoC3-Expr <- (recur e))
           (return-state (Id-Coercion-Huh e))]
          [(Fn-Coercion e* e)
           (e* : CoC3-Expr* <- (map-state recur e*))
           (e  : CoC3-Expr  <- (recur e))
           (return-state (Fn-Coercion e* e))]
          [(Fn-Coercion-Arg e1 e2)
           (e1 : CoC3-Expr  <- (recur e1))
           (e2 : CoC3-Expr  <- (recur e2))
           (return-state (Fn-Coercion-Arg e1 e2))]
          [(Fn-Coercion-Return e)
           (e  : CoC3-Expr  <- (recur e))
           (return-state (Fn-Coercion-Return e))]
          [(Ref-Coercion e1 e2)
           (e1 : CoC3-Expr  <- (recur e1))
           (e2 : CoC3-Expr  <- (recur e2))
           (return-state (Ref-Coercion e1 e2))]
          [(Ref-Coercion-Read e)
           (e  : CoC3-Expr  <- (recur e))
           (return-state (Ref-Coercion-Read e))]
          [(Ref-Coercion-Write e)
           (e  : CoC3-Expr  <- (recur e))
           (return-state (Ref-Coercion-Write e))]
          [(Type-Fn-arg e i)
           (e  : CoC3-Expr <- (recur e))
           (i  : CoC3-Expr <- (recur i))
           (return-state (Type-Fn-arg e i))]
          [(Type-Fn-return e)
           (e  : CoC3-Expr <- (recur e))
           (return-state (Type-Fn-return e))]
          [(Type-Fn-arity e)
           (e  : CoC3-Expr <- (recur e))
           (return-state (Type-Fn-arity e))]
          [(Blame e)
           (e  : CoC3-Expr <- (recur e))
           (return-state (Blame e))]
          [(If tst csq alt)
           (tst  : CoC3-Expr <- (recur tst))
           (csq  : CoC3-Expr <- (recur csq))
           (alt  : CoC3-Expr <- (recur alt))
           (return-state (If tst csq alt))]
          [(Var i) (return-state (Var i))]
          [(Type t) (return-state (Type t))]
          [(Quote k) (return-state (Quote k))]
          [(Quote-Coercion c) (return-state (Quote-Coercion c))]
          [(Begin exp* exp)
           (exp* : CoC3-Expr* <- (map-state recur exp*))
           (exp  : CoC3-Expr  <- (recur exp))
           (return-state (Begin exp* exp))]
          [(Repeat i e1 e2 e3)
           (e1 : CoC3-Expr <- (recur e1))
           (e2 : CoC3-Expr <- (recur e2))
           (e3 : CoC3-Expr <- (recur e3))
           (return-state (Repeat i e1 e2 e3))]
          [(Unguarded-Box exp)
           (exp  : CoC3-Expr  <- (recur exp))
           (return-state (Unguarded-Box exp))]
          [(Unguarded-Box-Ref exp)
           (exp  : CoC3-Expr  <- (recur exp))
           (return-state (Unguarded-Box-Ref exp))]
          [(Unguarded-Box-Set! exp1 exp2)
           (exp1 : CoC3-Expr  <- (recur exp1))
           (exp2 : CoC3-Expr  <- (recur exp2))
           (return-state (Unguarded-Box-Set! exp1 exp2))]
          [(Unguarded-Vect exp1 exp2)
           (exp1  : CoC3-Expr  <- (recur exp1))
           (exp2  : CoC3-Expr  <- (recur exp2))
           (return-state (Unguarded-Vect exp1 exp2))]
          [(Unguarded-Vect-Ref exp1 exp2)
           (exp1  : CoC3-Expr  <- (recur exp1))
           (exp2  : CoC3-Expr  <- (recur exp2))
           (return-state (Unguarded-Vect-Ref exp1 exp2))]
          [(Unguarded-Vect-Set! exp1 exp2 exp3)
           (exp1 : CoC3-Expr  <- (recur exp1))
           (exp2 : CoC3-Expr  <- (recur exp2))
           (exp3 : CoC3-Expr  <- (recur exp3))
           (return-state (Unguarded-Vect-Set! exp1 exp2 exp3))]
          [(Guarded-Proxy-Huh exp)
           (exp  : CoC3-Expr  <- (recur exp))
           (return-state (Guarded-Proxy-Huh exp))]
          [(Guarded-Proxy e r)
           (e : CoC3-Expr  <- (recur e))
           (match r
             [(Twosome t1 t2 l)
              (t1 : CoC3-Expr  <- (recur t1))
              (t2 : CoC3-Expr  <- (recur t2))
              (l  : CoC3-Expr  <- (recur l))
              (return-state (Guarded-Proxy e (Twosome t1 t2 l)))]
             [(Coercion c)
              (c  : CoC3-Expr  <- (recur c))
              (return-state (Guarded-Proxy e (Coercion c)))])]
          [(Guarded-Proxy-Ref exp)
           (exp  : CoC3-Expr  <- (recur exp))
           (return-state (Guarded-Proxy-Ref exp))]
          [(Guarded-Proxy-Source exp)
           (exp  : CoC3-Expr  <- (recur exp))
           (return-state (Guarded-Proxy-Source exp))]
          [(Guarded-Proxy-Target exp)
           (exp  : CoC3-Expr  <- (recur exp))
           (return-state (Guarded-Proxy-Target exp))]
          [(Guarded-Proxy-Blames exp)
           (exp  : CoC3-Expr  <- (recur exp))
           (return-state (Guarded-Proxy-Blames exp))]
          [(Guarded-Proxy-Coercion exp)
           (exp  : CoC3-Expr  <- (recur exp))
           (return-state (Guarded-Proxy-Coercion exp))]
          [other (error 'ic-expr "umatched ~a" other)])))
    (recur exp))

;; Interpret Casts recuring through a single binding
(: ic-bnd (IC-ExprT -> (CoC2-Bnd -> (State Nat CoC3-Bnd))))
(define ((ic-bnd recur) b)
  (match-let ([(cons u e) b])
    (bind-state
     (recur e)
     (lambda ([e : CoC3-Expr])
       : (State Nat CoC3-Bnd)
       (return-state (cons u e))))))

;; Interpret Casts recuring through a single code binding
(: ic-bndc (IC-ExprT -> (CoC2-Bnd-Code -> (State Nat CoC3-Bnd-Code))))
(define ((ic-bndc recur) c)
  (match-let ([(cons u (Code u* e)) c])
    (bind-state
     (recur e)
     (lambda ([e : CoC3-Expr])
       : (State Nat CoC3-Bnd-Code)
       (return-state (cons u (Code u* e)))))))
