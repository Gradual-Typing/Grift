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
 "../language/cast-or-coerce3.rkt")
(provide
 interpret-casts
 (all-from-out
  "../language/cast-or-coerce2.rkt"
  "../language/cast-or-coerce3.rkt"))

(: specialize-casts? (Parameterof Boolean))
(define specialize-casts?
  (make-parameter #f))

(: recursive-dyn-cast? (Parameterof Boolean))
(define recursive-dyn-cast?
  (make-parameter #t))

(: interpret-casts (Cast-or-Coerce2-Lang Config . -> . Cast-or-Coerce3-Lang))
(trace-define (interpret-casts prgm config)
              (match-let ([(Prog (list name next type) exp) prgm])
                (let-values ([(exp next) (run-state (ic-prgm exp type) next)])
                  (Prog (list name next type) exp))))

(: ic-prgm (CoC2-Expr Schml-Type -> (State Nat CoC3-Expr)))
(define (ic-prgm exp type)
  (do (bind-state : (State Nat CoC3-Expr))
      (interp-cast : Uid <- (uid-state "interp_cast"))
      (let* ([interp
              : Cast-Rule
              ;; This cast rule will delegate casts to the
              ;; routine that represents the runtimes interp
              ;; cast functioninterp : Cast-Rule
              (lambda (v t1 t2 l)
                (return-state
                 (App-Code (Code-Label interp-cast) (list v t1 t2 l))))]
             [spec
              : Cast-Rule
              ;; This cast rule will specialize casts base on
              ;; compile time informations of the types involved
              ;; in the cast.
              ;; The different versions differ in what to do
              ;; when casting from dyn
              (if (recursive-dyn-cast?)
                  (lambda (v t1 t2 l)
                    (cast-any interp v t1 t2 l))
                  (lambda (v t1 t2 l)
                    (cast-any spec-cast-undyned v t1 t2 l)))]
             [judge
              : Cast-Rule
              ;; This cast rule will specialize only specialize
              ;; if the input doesn't require additional computation
              (if (specialize-casts?)
                  (lambda (v t1 t2 l)
                    (if (or (Quote? v) (Type? t1) (Type? t2))
                        (spec v t1 t2 l)
                        (interp v t1 t2 l)))
                  interp)])
        
        ;; This is a little weird but leads to code reuse.
        ;; Notice invoking specialize on vars will cause the
        ;; entire cast decision tree to be built.
        ;; We then wrap up this entire tree as C function to
        ;; create the runtime routine that can cast any function
        (v : Uid <- (uid-state "val"))
        (t1 : Uid <- (uid-state "type1"))
        (t2 : Uid <- (uid-state "type2"))
        (l  : Uid <- (uid-state "label"))
        (cast-tree
         : CoC3-Expr
         <- (spec (Var v) (Var t1) (Var t2) (Var l)))
        (let* ([rt-cast      (Code `(,v ,t1 ,t2 ,l) cast-tree)]
               [rt-cast-bnd* `((,interp-cast . ,rt-cast))])
          (exp : CoC3-Expr <- ((ic-expr judge) exp))
          (return-state
           (Labels rt-cast-bnd*
            (Observe exp type)))))))

;; How to cast any type to some other type
(: cast-any Cast-Prim-Rule)
(define (cast-any cast-undyned v t1 t2 lbl)
  (let$* ([type1 t1] [type2 t2])
         (cond$
          [(op=? type1 type2) (return-state v)]
          [(op=? type1 (Type DYN-TYPE)) (cast-dyn cast-undyned v type1 type2 lbl)]
          [else (cast-ground v type1 type2 lbl)])))

;; How to extract a dynamic value
(: cast-dyn Cast-Prim-Rule)
(define (cast-dyn cast-undyned v t1 t2 lbl)
  (let$* ([val v] [tag (Dyn-tag val)])
         (cond$
          [(op=? (Tag 'Int) tag)
           (cast-undyned (Dyn-immediate val) (Type INT-TYPE) t2 lbl)]
          [(op=? (Tag 'Bool) tag)
           (cast-undyned (Dyn-immediate val) (Type BOOL-TYPE) t2 lbl)]
          [(op=? (Tag 'Unit) tag)
           (cast-undyned (Quote '()) (Type UNIT-TYPE) t2 lbl)]
          [(op=? (Tag 'Boxed) tag)
           (cast-undyned (Dyn-value val) (Dyn-type val) t2 lbl)]
          [else (return-state (Blame (Quote "Unexpected value in cast tree")))])))

;; How to cast any Injectable type to any other type
(: cast-ground Cast-Aux-Rule)
(define (cast-ground v t1 t2 lbl)
  (let$* ([type1 t1])
   (cond$
    [(op=? type1 (Type INT-TYPE))  (cast-int v type1 t2 lbl)]
    [(op=? type1 (Type BOOL-TYPE)) (cast-bool v type1 t2 lbl)]
    [(op=? type1 (Type UNIT-TYPE)) (cast-unit v type1 t2 lbl)]
    [else
     (let$* ([tag1 (type-tag type1)])
            (cond$
             [(op=? (Tag 'Fn) tag1) (cast-fn v type1 t2 lbl)]
             [(op=? (Tag 'GRef) tag1) (cast-gref v type1 t2 lbl)]
             [(op=? (Tag 'GVect) tag1) (cast-gvect v type1 t2 lbl)]
             [else
              (return-state
               (Blame (Quote "Unexpected Type1 in cast tree")))]))])))

;; How to cast an Int to Any type
(: cast-int Cast-Aux-Rule)
(define (cast-int v t1 t2 lbl)
  (let$* ([val v][type2 t2])
         (cond$
          [(op=? (Type DYN-TYPE) type2) (return-state (Dyn-make val (Type INT-TYPE)))]
          [else (return-state (Blame lbl))])))

;; How to Cast Unit to some other type
(: cast-unit Cast-Aux-Rule)
(define (cast-unit v t1 t2 lbl)
  (let$* ([val v][type2 t2])
         (cond$
          [(op=? (Type DYN-TYPE) type2)
           (return-state (Dyn-make val (Type UNIT-TYPE)))]
          [else (return-state (Blame lbl))])))

;; How to Cast Bool to some other type
(: cast-bool Cast-Aux-Rule)
(define (cast-bool v t1 t2 lbl)
  (let$* ([val v][type2 t2])
         (cond$
          [(op=? (Type DYN-TYPE) type2)
           (return-state (Dyn-make val (Type BOOL-TYPE)))]
          [else (return-state (Blame lbl))])))

;; How to cast a Guarded Reference to some other type
(: cast-gref Cast-Aux-Rule)
(define (cast-gref v t1 t2 lbl)
  (: gref-arg (CoC3-Expr -> CoC3-Expr))
  (define (gref-arg t)
    (match t
      [(Type (GRef a)) (Type a)]
      [other (Type-GRef-Of t)]))
  (: proxy-gref Cast-Aux-Rule)
  (define (proxy-gref val type1 type2 lbl)
    (let$* ([tag_gref (type-tag type2)])
           (if$ (op=? tag_gref (Tag 'GRef))
                (let$* ([g1 (gref-arg type1)]
                        [g2 (gref-arg type2)])
                       (return-state (Guarded-Proxy val (Twosome g1 g2 lbl))))
                (return-state (Blame lbl)))))
  (let$* ([val v] [type1 t1] [type2 t2])
         (if$ (op=? (Type DYN-TYPE) type2)
              (return-state (Dyn-make val t1))
              (proxy-gref val type1 type2 lbl))))

;; How to Cast a Guarded Vector to some other type
(: cast-gvect Cast-Aux-Rule)
(define (cast-gvect v t1 t2 lbl)
  (: gvect-arg (CoC3-Expr -> CoC3-Expr))
  (define (gvect-arg t)
    (match t
      [(Type (GVect a)) (Type a)]
      [other (Type-GVect-Of t)]))
  (: proxy-gvect Cast-Aux-Rule)
  (define (proxy-gvect val type1 type2 lbl)
    (let$* ([tag_gvect (type-tag type2)])
           (if$ (op=? tag_gvect (Tag 'GVect))
                (let$* ([g1 (gvect-arg type1)]
                        [g2 (gvect-arg type2)])
                       (return-state (Guarded-Proxy val (Twosome g1 g2 lbl))))
                (return-state (Blame lbl)))))
  (let$* ([val v] [type1 t1] [type2 t2])
         (if$ (op=? (Type DYN-TYPE) type2)
              (return-state (Dyn-make val t1))
              (proxy-gvect val type1 type2 lbl))))

;; How to Cast a Function to some other type
(: cast-fn Cast-Aux-Rule)
(define (cast-fn v t1 t2 lbl)
  (let$* ([type1 t1]
          [type2 t2])
   (if$ (op=? (Type DYN-TYPE) type2)
        (return-state (Dyn-make v type1))
        (let$* ([tag2 (type-tag type2)])
         (if$ (op=? tag2 (Tag 'Fn))
              (let$* ([type1_arity (type-fn-arity type1)]
                      [type2_arity (type-fn-arity type2)]
                      [value v])
               (if$ (op=? type1_arity type2_arity)
                    (return-state
                     (App-Code (Fn-Caster value)
                               (list value t1 type2 lbl)))
                    (return-state (Blame lbl))))
              (return-state (Blame lbl)))))))

;; How to cast a non dynamic value to another type
;; Notice this is used in conjuction with interpret cast based
;; on the setting of the parameter recursive-dyn-cast
;; when set to #t the cast used is a call to the interpret
;; cast runtime routine instead of unfolding the cast tree even
;; I am pretty sure this is the desired behavior
(: spec-cast-undyned Cast-Aux-Rule)
(define (spec-cast-undyned v t1 t2 lbl)
  ;; There is an invarient that there will only ever be one level of dyn
  (let$* ([type1 t1] [type2 t2])
         (cond$
          [(op=? type1 type2) (return-state v)]
          [else (cast-ground v type1 type2 lbl)])))

;; A Cast rule is part of the decision tree for allowed versus
;; not allowed casts. They use a few macros that keep invariants
;; managable and allow literals to prune the tree to only possible
;; needed branches
(define-type Cast-Rule (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr ->
                                  (State Nat CoC3-Expr)))

(define-type Cast-Prim-Rule
  (Cast-Rule CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> (State Nat CoC3-Expr)))

(define-type Cast-Aux-Rule
  (CoC3-Expr CoC3-Expr CoC3-Expr CoC3-Expr -> (State Nat CoC3-Expr)))


(define-type IC-Expr (CoC2-Expr -> (State Nat CoC3-Expr)))
(: ic-expr (-> Cast-Rule (-> CoC2-Expr (State Nat CoC3-Expr))))


(define ((ic-expr mk-cast) exp)
  (: recur (-> CoC2-Expr (State Nat CoC3-Expr)))
  (define (recur exp)
    (do (bind-state : (State Nat CoC3-Expr))
        (match exp
          ;; Interesting Transformations
          [(Interpreted-Cast v (Twosome t1 t2 l))
           (v  : CoC3-Expr <- (recur v))
           (t1 : CoC3-Expr <- (recur t1))
           (t2 : CoC3-Expr <- (recur t2))
           (l  : CoC3-Expr <- (recur l))
           (mk-cast v t1 t2 l)]
          [(Cast v (Twosome t1 t2 l))
           (v  : CoC3-Expr <- (recur v))
           (mk-cast v (Type t1) (Type t2) (Quote l))]
          [(App/Fn-Proxy-Huh e e*) (error 'ic-todo)]
          [(Compose-Coercions c1 c2)
           (c1 : CoC3-Expr <- (recur c1))
           (c2 : CoC3-Expr <- (recur c2))
           (error 'ic-todo)]
          [other (ic-expr-boring-cases other)])))
  ;; Thiλs function takes care of the boring cases
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
          [(Fn-Proxy i e1 e2)
           (e1 : CoC3-Expr <- (recur e1))
           (e2 : CoC3-Expr <- (recur e2))
           (return-state (Fn-Proxy i e1 e2))]
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
          [(Guarded-Proxy e1 (Twosome e2 e3 e4))
           (e1 : CoC3-Expr  <- (recur e1))
           (e2 : CoC3-Expr  <- (recur e2))
           (e3 : CoC3-Expr  <- (recur e3))
           (e4 : CoC3-Expr  <- (recur e4))
           (return-state (Guarded-Proxy e1 (Twosome e2 e3 e4)))]
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
          [other (error 'ic-expr "~a" other)])))
    (recur exp))

(: ic-bnd (IC-Expr -> (CoC2-Bnd -> (State Nat CoC3-Bnd))))
(define ((ic-bnd recur) b)
  (match-let ([(cons u e) b])
    (bind-state
     (recur e)
     (lambda ([e : CoC3-Expr])
       : (State Nat CoC3-Bnd)
       (return-state (cons u e))))))

(: ic-bndc (IC-Expr -> (CoC2-Bnd-Code -> (State Nat CoC3-Bnd-Code))))
(define ((ic-bndc recur) c)
  (match-let ([(cons u (Code u* e)) c])
    (bind-state
     (recur e)
     (lambda ([e : CoC3-Expr])
       : (State Nat CoC3-Bnd-Code)
       (return-state (cons u (Code u* e)))))))



  ;;These functions type to fold predicates in order to allow ifs to
  ;;generate only checks that are actually needed
  (: type-tag (-> CoC3-Expr CoC3-Expr))
  (define (type-tag o)
    (if (Type? o)
        (let ([v (Type-type o)])
          (cond
            [(or (Dyn? v) (Int? v) (Bool? v))
             (Tag 'Atomic)]
            [(GRef? v) (Tag 'GRef)]
            [(GVect? v) (Tag 'GVect)]
            [(Fn? v) (Tag 'Fn)]
            [else (error 'interpret-casts/type-tag
                         "Unexpected ~a" v)]))
        (Type-Tag o)))

  (: type-fn-arity (CoC3-Expr -> CoC3-Expr))
  (define (type-fn-arity o)
    (cond
      [(not (Type? o)) (Type-Fn-arity o)]
      [(Fn? (Type-type o)) (Quote (Fn-arity (Type-type o)))]
      [else (error 'ic/type-fn-arity "bad value?: ~a" o)]))

  ;; performs compile time folding of prim = on literals
  (: op=? (-> CoC3-Expr CoC3-Expr CoC3-Expr))
  (define (op=? o x)
    (cond
      [(and (Quote? o) (Quote? x) (Quote (eq? (Quote-literal o)
                                              (Quote-literal x))))]
      [(and (Tag? o) (Tag? x)) (Quote (eq? (Tag-bits o) (Tag-bits x)))]
      [(and (Type? o) (Type? x)) (Quote (equal? (Type-type o) (Type-type x)))]
      [else (Op '= (list o x))]))

  ;; construct new type literals based on the fn-ref operation
  #;(: fn-ref (-> CoC3-Expr (U Index 'return 'arity) CoC3-Expr))
  #;(define (fn-ref t i)
  (if (Type? t)
  (let ((t (Type-type t)))
  (if (Fn? t)
  (cond
    [(eq? i 'return) (Type (Fn-ret t))]
    [(eq? i 'arity)  (Quote (Fn-arity t))]
    [else (Type ((inst list-ref Schml-Type) (Fn-fmls t) i))])
  (error 'fn-ref "generated a function ref for non function type")))
(cond
  [(eq? i 'return) (Type-Fn-return t)]
  [(eq? i 'arity)  (Type-Fn-arity t)]
  [else (Type-Fn-arg t (Quote i))])))

;; if$ and cond$ will prune branches if the condition is (Quote #t) or (Quote #f)
;; and generates If statements (to construct the cast tree)
(: if$ (CoC3-Expr (State Nat CoC3-Expr) (State Nat CoC3-Expr) -> (State Nat CoC3-Expr)))
(define (if$ t c a)
  (if (Quote? t)
      (if (Quote-literal t)
          c
          a)
      (do (bind-state : (State Nat CoC3-Expr))
          (tmp-c : CoC3-Expr <- c)
        (tmp-a : CoC3-Expr <- a)
        (return-state (If t tmp-c tmp-a)))))

(define-syntax cond$
  (syntax-rules (else)
    [(_ (else c)) c]
    [(_ (t c) (t* c*) ...)
     (if$ t c (cond$ (t* c*) ...))]))

;; make sure that t is a trivial expression
(define-type Trivial (U (Quote Cast-Literal)
                        (Tag Tag-Symbol)
                        (Type Schml-Type)
                        (Var Uid)))

;(: trivial? (CoC3-Expr -> Boolean : Trivial))
(define (trivial? x)
  (or (Quote? x)
      (Tag? x)
      (Type? x)
      (Var? x)))

(: trivialize (String CoC3-Expr CoC3-Bnd* ->
                      (State Nat (Pair CoC3-Bnd* Trivial))))
(define (trivialize s e b*)
  (if (or (Quote? e) (Tag? e) (Type? e) (Var? e))
      (return-state (cons b* e))
      (bind-state
       (uid-state s)
       (lambda ([u : Uid])
         : (State Nat (Pair CoC3-Bnd* (Var Uid)))
         (return-state `(((,u . ,e) . ,b*) . ,(Var u)))))))

;; creates CoC3-Expr let bindings for non-trivial CoC3-Expr expressions,
;; since non-trivial expressions must be evaluated once.
(define-syntax let$*
  (syntax-rules ()
    [(_ () b) b]
    [(_ ([t* v*] ...) b)
     ;; I have had some major typechecking problems with sites that
     ;; have multiple cases of this code. On way around this would be
     ;; to stop this nonsense and to make sure that every expression
     ;; is fully evaluated before casting. The thought would then be
     ;; that some uneeded evaluations wouldn't be remove by eliminating
     ;; uneeded results later.
     ;; Another way would be to always allocate the uvar and just throw
     ;; it away if it turns out that the thing is already a trivial value.
     (let ([b* : CoC3-Bnd* '()])
       (do (bind-state : (State Nat CoC3-Expr))
           ((cons b* t*) : (Pair CoC3-Bnd* Trivial)
            <- (trivialize (~a 't*) v* b*)) ...
           (b^ : CoC3-Expr <- b)
           (if (null? b*)
               (return-state b^)
               (return-state (Let b* b^)))))]))

#|
#;(let ((tmp : CoC3-Expr v))
     (if (c3-trivial? tmp)
     ;;if the expression is a terminal then just update the binding
     (let ((t : CoC3-Expr tmp)) (let$* ([t* v*] ...) b))
     ;;if the expression is non terminal bind a var and bind the
     ;;expression to the var
     (do (bind-state : (State Nat CoC3-Expr))
         (u : Uid <- (uid-state (~a 't)))
       (let* ([t : CoC3-Expr (Var u)])
         (body : CoC3-Expr <- (let$* ([t* v*] ...) b))
         (return-state (Let (list (cons u tmp)) body))))))]))
#;
(let-values ([(uid n) (next-uid (~a 't) n)])
  (let ([t : CoC3-Expr (Var uid)])
    (let-values ([(body n) (let$* n ([t* v*] ...) b)])
      (values (Let (list (cons uid tmp)) body) n))))
|#
