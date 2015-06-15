#lang typed/racket
#|-----------------------------------------------------------------------------+
|Pass: src/casts/interpret-casts                                          |
+------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                             |
+------------------------------------------------------------------------------+
|Discription:
 -This pass creates a cast-interpreter that can cast arbitrary values at
  runtime.
 -Coverts runtime casts to applications of the cast-interpreter.
 -Specializes casts with known types, at this point represented with the cast
  form, to the shortest branch of the cast tree that is relevant.
 -Replaces types that are structure with a psuedo constructor call
 -Replaces tags atomic types with the Type form.
 -Introduces the short lived dynamic type manipulation forms
+------------------------------------------------------------------------------+
|Input Grammar Cast2-Language                                                  |
|Output Grammar Cast3-Language                                                 |
+-----------------------------------------------------------------------------|#
;; The define-pass syntax
(require "../helpers.rkt"
         "../errors.rkt"
         "../language.rkt")

;; Only the pass is provided by this module
(provide interpret-casts)

(: specialize-casts? (Parameterof Boolean))
(define specialize-casts?
  (make-parameter #f))

(: recursive-dyn-cast? (Parameterof Boolean))
(define recursive-dyn-cast?
  (make-parameter #t))



(: interpret-casts (Cast2-Lang Config . -> . Cast3-Lang))
(trace-define (interpret-casts prgm config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let-values ([(exp next) (run-state (ic-prgm exp type) next)])
      (Prog (list name next type) exp))))

(: ic-prgm (C2-Expr Schml-Type -> (State Nat C3-Expr)))
(define (ic-prgm exp type)
  (do (bind-state : (State Nat C3-Expr))
      (cst : Uid     <- (uid-state "interp_cast"))
      (let* ([interp (mk-interped-caster cst)]
             [spec   (mk-specialized-caster interp)]
             [judge  (mk-judicious-caster spec interp)])
        (bnd : C3-Bnd* <- (mk-cast-interp cst spec))
        (exp : C3-Expr <- ((ic-expr judge) exp))
        (return-state (Letrec bnd (Observe exp type))))))

#;
(let*-values ([(interp-cast interp-bnd next) (mk-cast-interp next)]
              [(exp next) ((ic-expr interp-cast) exp next)]
              [(prog-uid next) (next-uid "prog_returns" next)]
              [(prog-bnd) (cons prog-uid exp)]
              [(prog-var) (Var prog-uid)])
  )

(: mk-cast-interp (Uid Cast-Rule -> (State Nat C3-Bnd*)))
(define (mk-cast-interp name spec)
  (do (bind-state : (State Nat C3-Bnd*))
      (rhs : C3-Expr <- (finalize-interp-code spec))
      (let ([bnd : C3-Bnd (cons name rhs)])
        (return-state (list bnd)))))
#;
(let*-values ([(interp-uid next) (next-uid "interp_cast" next)]
              [(interp-var) (Var interp-uid)]
              [(interp-fn next) (finalize-interp-code interp-var next)])
  (values (interp-cast interp-var) (list (cons interp-uid interp-fn)) next))

(: finalize-interp-code (Cast-Rule -> (State Nat C3-Expr)))
(define (finalize-interp-code spec)
  (do (bind-state : (State Nat C3-Expr))
      (v  : Uid <- (uid-state "val"))
      (t1 : Uid <- (uid-state "type1"))
      (t2 : Uid <- (uid-state "type2"))
      (l  : Uid <- (uid-state "label"))
      (full-cast-tree  : C3-Expr
        <- (spec (Var v) (Var t1) (Var t2) (Var l)))
      (return-state
       (Lambda (list v t1 t2 l)
        (Castable #f full-cast-tree)))))

(: ic-expr (-> Cast-Rule (-> C2-Expr (State Nat C3-Expr))))
(define (ic-expr mk-cast)
  (: ic-bnd (-> C2-Bnd (State Nat C3-Bnd)))
  (define (ic-bnd b)
    (do (bind-state : (State Nat C3-Bnd))
        (match-let ([(cons u e) b])
          (e : C3-Expr <- (recur e))
          (return-state (cons u e)))))
  (: recur (-> C2-Expr (State Nat C3-Expr)))
  (define (recur exp)
    (match exp
      [(Lambda f* (Castable ctr exp))
       (do (bind-state : (State Nat C3-Expr))
           (exp : C3-Expr <- (recur exp))
           (return-state (Lambda f* (Castable ctr exp))))]
      [(Letrec b* exp)
       (do (bind-state : (State Nat C3-Expr))
           (b*  : C3-Bnd* <- (map-state ic-bnd b*))
           (exp : C3-Expr <- (recur exp))
           (return-state (Letrec b* exp)))]
      [(Let b* exp)
       (do (bind-state : (State Nat C3-Expr))
           (b*  : C3-Bnd* <- (map-state ic-bnd b*))
           (exp : C3-Expr <- (recur exp))
           (return-state (Let b* exp)))]
      [(App exp exp*)
       (do (bind-state : (State Nat C3-Expr))
           (exp  : C3-Expr  <- (recur exp))
           (exp* : C3-Expr* <- (map-state recur exp*))
           (return-state (App exp exp*)))]
      [(Op p exp*)
       (do (bind-state : (State Nat C3-Expr))
           (exp* : C3-Expr* <- (map-state recur exp*))
           (return-state (Op p exp*)))]
      [(Runtime-Cast v t1 t2 l)
       (do (bind-state : (State Nat C3-Expr))
           (v  : C3-Expr <- (recur v))
           (t1 : C3-Expr <- (recur t1))
           (t2 : C3-Expr <- (recur t2))
           (l  : C3-Expr <- (recur l))
           (mk-cast v t1 t2 l))]
      [(Cast v t1 t2 l)
       (do (bind-state : (State Nat C3-Expr))
           (v  : C3-Expr <- (recur v))
           (mk-cast v (Type t1) (Type t2) (Quote l)))]
      [(Fn-Cast e t1 t2 l)
       (do (bind-state : (State Nat C3-Expr))
           (e  : C3-Expr <- (recur e))
           (cast-fn e (Type t1) (Type t2) (Quote l)))]
      [(Type-Fn-arg e i)
       (do (bind-state : (State Nat C3-Expr))
           (e  : C3-Expr <- (recur e))
           (i  : C3-Expr <- (recur i))
           (return-state (Type-Fn-arg e i)))]
      [(Type-Fn-return e)
       (do (bind-state : (State Nat C3-Expr))
           (e  : C3-Expr <- (recur e))
           (return-state (Type-Fn-return e)))]
      [(Type-Fn-arity e)
       (do (bind-state : (State Nat C3-Expr))
           (e  : C3-Expr <- (recur e))
           (return-state (Type-Fn-arity e)))]
      [(Blame e)
       (do (bind-state : (State Nat C3-Expr))
           (e  : C3-Expr <- (recur e))
           (return-state (Blame e)))]
      [(If tst csq alt)
       (do (bind-state : (State Nat C3-Expr))
           (tst  : C3-Expr <- (recur tst))
           (csq  : C3-Expr <- (recur csq))
           (alt  : C3-Expr <- (recur alt))
           (return-state (If tst csq alt)))]
      [(Var i) (return-state (Var i))]
      [(Type t) (return-state (Type t))]
      [(Quote k) (return-state (Quote k))]
      [(Begin exp* exp)
       (do (bind-state : (State Nat C3-Expr))
           (exp* : C3-Expr* <- (map-state recur exp*))
           (exp  : C3-Expr  <- (recur exp))
           (return-state (Begin exp* exp)))]
      [(Repeat i e1 e2 e3)
       (do (bind-state : (State Nat C3-Expr))
           (e1 : C3-Expr <- (recur e1))
           (e2 : C3-Expr <- (recur e2))
           (e3 : C3-Expr <- (recur e3))
           (return-state (Repeat i e1 e2 e3)))]
      [(UGbox exp)
       (do (bind-state : (State Nat C3-Expr))
           (exp  : C3-Expr  <- (recur exp))
           (return-state (UGbox exp)))]
      [(UGbox-ref exp)
       (do (bind-state : (State Nat C3-Expr))
           (exp  : C3-Expr  <- (recur exp))
           (return-state (UGbox-ref exp)))]
      [(UGbox-set! exp1 exp2)
       (do (bind-state : (State Nat C3-Expr))
           (exp1 : C3-Expr  <- (recur exp1))
           (exp2 : C3-Expr  <- (recur exp2))
           (return-state (UGbox-set! exp1 exp2)))]
      [(GRep-proxied? exp)
       (do (bind-state : (State Nat C3-Expr))
           (exp  : C3-Expr  <- (recur exp))
           (return-state (GRep-proxied? exp)))]
      [(Gproxy e1 e2 e3 e4)
       (do (bind-state : (State Nat C3-Expr))
           (e1 : C3-Expr  <- (recur e1))
           (e2 : C3-Expr  <- (recur e2))
           (e3 : C3-Expr  <- (recur e3))
           (e4 : C3-Expr  <- (recur e4))
           (return-state (Gproxy e1 e2 e3 e4)))]
      [(Gproxy-for exp)
       (do (bind-state : (State Nat C3-Expr))
           (exp  : C3-Expr  <- (recur exp))
           (return-state (Gproxy-for exp)))]
      [(Gproxy-from exp)
       (do (bind-state : (State Nat C3-Expr))
           (exp  : C3-Expr  <- (recur exp))
           (return-state (Gproxy-from exp)))]
      [(Gproxy-to exp)
       (do (bind-state : (State Nat C3-Expr))
           (exp  : C3-Expr  <- (recur exp))
           (return-state (Gproxy-to exp)))]
      [(Gproxy-blames exp)
       (do (bind-state : (State Nat C3-Expr))
           (exp  : C3-Expr  <- (recur exp))
           (return-state (Gproxy-blames exp)))]))
  recur)

;; A Cast rule is part of the decision tree for allowed versus
;; not allowed casts. They use a few macros that keep invariants
;; managable and allow literals to prune the tree to only possible
;; needed branches
(define-type Cast-Rule (C3-Expr C3-Expr C3-Expr C3-Expr ->
                        (State Nat C3-Expr)))

;; Build a custom cast decision tree
(: mk-specialized-caster (-> Cast-Rule Cast-Rule))
(define (mk-specialized-caster interp)
  (define rec-cast : Cast-Rule
    (if (recursive-dyn-cast?) interp spec-cast-undyned))
  (: specialized-caster Cast-Rule)
  (define (specialized-caster v t1 t2 l)
    (cast-any rec-cast v t1 t2 l))
  specialized-caster)

;; Invoke the prebuild tree as a function
(: mk-interped-caster (-> Uid Cast-Rule))
(define (mk-interped-caster interp-uid)
  (define interp-var (Var interp-uid))
  (: interped-caster Cast-Rule)
  (define (interped-caster v t1 t2 l)
    (return-state (App interp-var (list v t1 t2 l))))
  interped-caster)

(: mk-judicious-caster (-> Cast-Rule Cast-Rule Cast-Rule))
(define (mk-judicious-caster spec interp)
  (: spec^ Cast-Rule)
  (define (spec^ v t1 t2 l)
    (if (or (Quote? v) (Type? t1) (Type? t2))
        (spec v t1 t2 l)
        (interp v t1 t2 l)))
  (if (specialize-casts?)
      spec^
      interp))




;;These functions type to fold predicates in order to allow ifs to
;;generate only checks that are actually needed
(: type-tag (-> C3-Expr C3-Expr))
(define (type-tag o)
  (if (Type? o)
      (let ([v (Type-type o)])
        (cond
         [(or (Dyn? v) (Int? v) (Bool? v))
          (Tag 'Atomic)]
         [(GRef? v) (Tag 'GRef)]
         [(Fn? v) (Tag 'Fn)]
         [else (error 'interpret-casts/type-tag
                      "Unexpected ~a" v)]))
      (Type-tag o)))

;; performs compile time folding of prim = on literals
(: op=? (-> C3-Expr C3-Expr C3-Expr))
(define (op=? o x)
  (cond
   [(and (Quote? o) (Quote? x) (Quote (eq? (Quote-literal o)
                                           (Quote-literal x))))]
   [(and (Tag? o) (Tag? x)) (Quote (eq? (Tag-bits o) (Tag-bits x)))]
   [(and (Type? o) (Type? x)) (Quote (equal? (Type-type o) (Type-type x)))]
   [else (Op '= (list o x))]))

;; construct new type literals based on the fn-ref operation
#;(: fn-ref (-> C3-Expr (U Index 'return 'arity) C3-Expr))
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
(: if$ (C3-Expr (State Nat C3-Expr) (State Nat C3-Expr) -> (State Nat C3-Expr)))
(define (if$ t c a)
  (if (Quote? t)
      (if (Quote-literal t)
          c
          a)
      (do (bind-state : (State Nat C3-Expr))
          (tmp-c : C3-Expr <- c)
          (tmp-a : C3-Expr <- a)
          (return-state (If t tmp-c tmp-a)))))

(define-syntax cond$
  (syntax-rules (else)
    [(_ (else c)) c]
    [(_ (t c) (t* c*) ...)
     (if$ t c (cond$ (t* c*) ...))]))

;; make sure that t is a terminal expression
;; expects to be used in the store passing style expressions
(define-syntax let$*
  (syntax-rules ()
    [(_ () b) b]
    [(_ ([t v] [t* v*] ...) b)
     (let ((tmp : C3-Expr v))
       (if (or (Quote? tmp) (Tag? tmp) (Type? tmp) (Var? tmp))
           ;;if the expression is a terminal then just update the binding
           (let ((t : C3-Expr tmp)) (let$* ([t* v*] ...) b))
           ;;if the expression is non terminal bind a var and bind the
           ;;expression to the var
           (do (bind-state : (State Nat C3-Expr))
               (u : Uid <- (uid-state (~a 't)))
               (let* ([t : C3-Expr (Var u)])
                 (body : C3-Expr <- (let$* ([t* v*] ...) b))
                 (return-state (Let (list (cons u tmp)) body))))))]))
#;
           (let-values ([(uid n) (next-uid (~a 't) n)])
             (let ([t : C3-Expr (Var uid)])
               (let-values ([(body n) (let$* n ([t* v*] ...) b)])
                 (values (Let (list (cons uid tmp)) body) n))))


(define-type Cast-Prim-Rule
  (Cast-Rule C3-Expr C3-Expr C3-Expr C3-Expr -> (State Nat C3-Expr)))

(define-type Cast-Aux-Rule
  (C3-Expr C3-Expr C3-Expr C3-Expr -> (State Nat C3-Expr)))

(: cast-any Cast-Prim-Rule)
(define (cast-any cast-undyned v t1 t2 lbl)
  (let$* ([type1 t1] [type2 t2])
   (cond$
    [(op=? type1 type2) (return-state v)]
    [(op=? type1 (Type DYN-TYPE)) (cast-dyn cast-undyned v type1 type2 lbl)]
    [else (cast-ground v type1 type2 lbl)])))

(: cast-dyn Cast-Prim-Rule)
(define (cast-dyn cast-undyned v t1 t2 lbl)
  (let$* ([val v] [tag (Dyn-tag val)])
   (cond$
    [(op=? (Tag 'Int) tag)
     (cast-undyned (Dyn-immediate val) (Type INT-TYPE) t2 lbl)]
    [(op=? (Tag 'Bool) tag)
     (cast-undyned (Dyn-immediate val) (Type BOOL-TYPE) t2 lbl)]
    [(op=? (Tag 'Unit) tag)
     (cast-undyned (Quote '()) (Type BOOL-TYPE) t2 lbl)]
    [(op=? (Tag 'Boxed) tag)
     (cast-undyned (Dyn-value val) (Dyn-type val) t2 lbl)]
    [else (return-state (Blame (Quote "Unexpected value in cast tree")))])))

(: spec-cast-undyned Cast-Aux-Rule)
(define (spec-cast-undyned v t1 t2 lbl)
  ;; There is an invarient that there will only ever be one level of dyn
  (let$* ([type1 t1] [type2 t2])
   (cond$
    [(op=? type1 type2) (return-state v)]
    [else (cast-ground v type1 type2 lbl)])))

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
         [else
          (return-state
           (Blame (Quote "Unexpected Type1 in cast tree")))]))])))

#;(TODO do something more clever than the current boxing schema)
(: cast-int Cast-Aux-Rule)
(define (cast-int v t1 t2 lbl)
 (let$* ([val v][type2 t2])
  (cond$
   ;;[(op=? (Type INT-TYPE) type2) (return-state val)]
   [(op=? (Type DYN-TYPE) type2) (return-state (Dyn-make val (Type INT-TYPE)))]
   [else (return-state (Blame lbl))])))

(: cast-unit Cast-Aux-Rule)
(define (cast-unit v t1 t2 lbl)
  (let$* ([val v][type2 t2])
  (cond$
   ;; this is likely uneeded
   ;;[(op=? (Type UNIT-TYPE) type2) (return-state val)]
   [(op=? (Type DYN-TYPE) type2)
    (return-state (Dyn-make val (Type UNIT-TYPE)))]
   [else (return-state (Blame lbl))])))

(: cast-bool Cast-Aux-Rule)
(define (cast-bool v t1 t2 lbl)
  (let$* ([val v][type2 t2])
  (cond$
   ;; this is probabaly uneeded
   ;;[(op=? (Type BOOL-TYPE) type2) (return-state val)]
   [(op=? (Type DYN-TYPE) type2)
    (return-state (Dyn-make val (Type BOOL-TYPE)))]
   [else (return-state (Blame lbl))])))

(: cast-gref Cast-Aux-Rule)
(define (cast-gref v t1 t2 lbl)
  (: gref-arg (C3-Expr -> C3-Expr))
  (define (gref-arg t)
    (match t
      [(Type (GRef a)) (Type a)]
      [other (Type-GRef-to t)]))
  (let$* ([val v] [type1 t1] [type2 t2])
   (if$ (op=? (Type DYN-TYPE) type2)
        (return-state (Dyn-make val t1))
        (let$* ([tag_gref (type-tag type2)])
         (if$ (op=? tag_gref (Tag 'GRef))
              (let$* ([g1 (gref-arg type1)]
                      [g2 (gref-arg type2)])
               (return-state (Gproxy val g1 g2 lbl)))
              (return-state (Blame lbl)))))))

(: cast-fn Cast-Aux-Rule)
(define (cast-fn v t1 t2 lbl)
  (let$* ([type2 t2])
    (if$ (op=? (Type DYN-TYPE) type2)
         (return-state (Dyn-make v t1))
         (let$* ([tag2 (type-tag type2)])
          (if$ (op=? tag2 (Tag 'Fn))
               (let$* ([value v])
                (return-state (App (Fn-Caster value) (list value t1 type2 lbl))))
               (return-state (Blame lbl)))))))
