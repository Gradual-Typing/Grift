#lang typed/racket/base/no-check
#|------------------------------------------------------------------------------+
|Pass: src/data/normalize-context                                          |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
| Description: This pass introduces the concept of keeping track
| of the context of an ast node.
+-------------------------------------------------------------------------------+
| Grammer:
+------------------------------------------------------------------------------|#
;; The define-pass syntax
(require
 "../../helpers.rkt"
 "../../errors.rkt"
 "../../configuration.rkt"
 "../../language/forms.rkt"
 "../../language/primitives.rkt"
 "../../casts/constants-and-codes.rkt"
 "../../language/make-begin.rkt")

;; Only the pass is provided by this module
(provide normalize-context)

(: normalize-context (-> Data0-Lang Data1-Lang))
(define (normalize-context prgm)
  (match-let ([(Prog (list name count type) (GlobDecs d* prog)) prgm])
    (let* ([bndc* : (Boxof D1-Bnd-Code*) (box '())]
           [prog  (nc-prog bndc* prog)]
           [bndc* : D1-Bnd-Code* (unbox bndc*)])
      (Prog (list name count type) (GlobDecs d* (Labels bndc* prog))))))

(: nc-prog ((Boxof D1-Bnd-Code*) D0-Expr -> D1-Tail))
(define (nc-prog lifted-code* prog)  
  ;; Make sure the expression is in tail possition
  (: nc-tail (D0-Expr -> D1-Tail))
  (define (nc-tail exp)
    (match exp
      [(Labels bndc* exp)
       (nc-bnd-code* bndc*)
       (nc-tail exp)]
      [(Let bnd* tail)
       (Let (nc-bnd* bnd*) (nc-tail tail))]
      [(If t c a)
       (If (nc-pred t) (nc-tail c) (nc-tail a))]
      [(Switch e c* d)
       (Switch (nc-value e) (map-switch-case* nc-tail c*) (nc-tail d))]
      [(Begin eff* exp)
       (make-begin (nc-effect* eff*) (nc-tail exp))]
      [(Repeat i e1 e2 a e3 e4)
       ;; This breaks SSA but we don't rely on SSA currently
       (Begin
         (list
          (Assign a (nc-value e3))
          (Repeat i (nc-value e1) (nc-value e2) #f #f
                  (Assign a (nc-value e4))))
         (Var a))]
      [(While e1 e2) (While (nc-pred e1) (nc-tail e2))]
      [(App-Code exp exp*)
       (App-Code (nc-value exp) (nc-value* exp*))]
      [(Op p (app nc-value* v*)) (nc-value-op p v*)]
      [(and (Stack-Alloc _) a) a]
      [(and (Var _) v) v]
      [(and (Global s) g) g]
      [(and (Code-Label _) l) l]
      [(and (Quote _) k) k]
      [(and (Halt) h) h]
      [(and (Success) s) s]
      ;; FIXME: Come up with a more compelling intermediate language
      [(No-Op) (Success)]
      ;; forms that don't quite work out because normalize
      ;; context is after specify
      ;; I could do
      [(Assign u e) (error 'nc-tail "unhandled case")]
      [other (error 'normalize-context "umatched ~a" other)]))
  (: nc-value (D0-Expr -> D1-Value))
  (define (nc-value exp)
    (match exp
      [(Labels bndc* exp)
       (nc-bnd-code* bndc*)
       (nc-value exp)]
      [(Let bnd* exp)
       (Let (nc-bnd* bnd*) (nc-value exp))]
      [(If t c a)
       (If (nc-pred t) (nc-value c) (nc-value a))]
      [(Switch e c* d)
       (Switch (nc-value e) (map-switch-case* nc-value c*) (nc-value d))]
      [(Begin eff* exp)
       (Begin (nc-effect* eff*) (nc-value exp))]
      [(Repeat i e1 e2 a e3 e4)
       (Begin
         (list
          (Assign a (nc-value e3))
          (Repeat i (nc-value e1) (nc-value e2) #f #f
                  (Assign a (nc-value e4))))
         (Var a))]
      [(While e1 e2) (While (nc-pred e1) (nc-value e2))]
      [(Break-Repeat) (Begin (list (Break-Repeat)) UNIT-IMDT)]
      [(App-Code exp exp*)
       (App-Code (nc-value exp) (nc-value* exp*))]
      [(Op p (app nc-value* v*)) (nc-value-op p v*)]
      [(and (Stack-Alloc _) a) a]
      [(Var i)  (Var i)]
      [(Global s) (Global s)]
      [(Code-Label i) (Code-Label i)]
      [(Quote k) (Quote k)]
      [(Halt) (Halt)]
      [(Assign u (app nc-value v))
       (Begin (list (Assign u v)) UNIT-IMDT)]
      [(No-Op) (error 'normalize-context "no-op in value context")]
      [other (error 'normalize-context "umatched ~a" other)]))
  (: nc-effect (-> D0-Expr D1-Effect))
  (define (nc-effect exp)
    (match exp
      [(Assign u (app nc-value v))
       (Assign u v)]
      [(Labels bndc* exp)
       (nc-bnd-code* bndc*)
       (nc-effect exp)]
      [(Let (app nc-bnd* bnd*) (app nc-effect tail))
       (Let bnd* tail)]
      [(If (app nc-pred t) (app nc-effect c) (app nc-effect a))
       (If t c a)]
      [(Switch e c* d)
       (Switch (nc-value e) (map-switch-case* nc-effect c*) (nc-effect d))]
      [(Begin eff* exp)
       (Begin (append (nc-effect* eff*) (list (nc-effect exp))) NO-OP)]
      [(Repeat i e1 e2 a e3 e4)
       (Begin
         (list
          (Assign a (nc-value e3))
          (Repeat i (nc-value e1) (nc-value e2) #f #f
                  (Assign a (nc-value e4))))
         NO-OP)]
      [(While e1 e2) (Begin (list (While (nc-pred e1) (nc-effect e2))) NO-OP)]
      [(Break-Repeat) (Break-Repeat)]
      [(App-Code exp exp*)
       (App-Code (nc-value exp) (nc-value* exp*))]
      [(Op p exp*)
       (if (grift-primitive-effect? p)
           (Op p (nc-value* exp*))
           ;; evaluate values for their effects
           (make-begin (nc-effect* exp*) NO-OP))]
      [(Stack-Alloc _) NO-OP]
      [(Var i)  NO-OP]
      [(Global s) NO-OP]
      [(Code-Label i) NO-OP]
      [(Quote k) NO-OP]
      [(No-Op) NO-OP]
      [other (error 'normalize-context "umatched ~a" other)]))
  (: nc-pred (-> D0-Expr D1-Pred))
  (define (nc-pred exp)
    (match exp
      [(Labels bndc* exp)
       (nc-bnd-code* bndc*)
       (nc-pred exp)]
      [(If (app nc-pred t) (app nc-pred c) (app nc-pred a))
       (If t c a)]
      [(Switch e c* d)
       (Switch (nc-value e) (map-switch-case* nc-pred c*) (nc-pred d))]
      [(Begin eff* exp)
       (Begin (nc-effect* eff*) (nc-pred exp))]
      [(Repeat i e1 e2 a e3 e4)
       (Begin
         (list
          (Assign a (nc-value e3))
          (Repeat i (nc-value e1) (nc-value e2) #f #f
                  (Assign a (nc-value e4))))
         (Relop '= (list TRUE-IMDT (Var a))))]
      [(While e1 e2) (While (nc-pred e1) (nc-pred e2))]
      [(Break-Repeat) (error 'nc-pred/unsuported/break-repeat)]
      [(Assign u e) (error 'nc-pred/unsuported/assign)]
      [(Op p (app nc-value* val*))
       (cond
         [(or (primitive-relop? p) (primitive-int? p) (array-ref? p))
          (Relop p val*)]
         [(primitive-bottom? p)
          (Begin (list (Op p val*)) TRUE-IMDT)]
         [else
          (error 'grift/normalize-context/nc-pred "unexpected relop: ~a" p)])]
      [(No-Op) (error 'nc-pred "no-op in pred context")]
      [(app nc-value v) (Relop '= (list TRUE-IMDT v))]))

(: nc-value* (-> D0-Expr* D1-Value*))
(define (nc-value* exp*) (map nc-value exp*))
  (: nc-effect* (-> D0-Expr* D1-Effect*))
  (define (nc-effect* exp*) (map nc-effect exp*))
  (: nc-bnd* (-> D0-Bnd* D1-Bnd*))
  (define (nc-bnd* bnd*)
    (map (lambda ([b : D0-Bnd])
           (cons (car b) (nc-value (cdr b))))
         bnd*))
  (: nc-bnd-code* (-> D0-Bnd-Code* Void))
  (define (nc-bnd-code* bnd*)
    (for-each
     (lambda ([b : D0-Bnd-Code])
       (match-let ([(cons u (Code u* t)) b])
         (let ([b (cons u (Code u* (nc-tail t)))])
           (set-box! lifted-code* (cons b (unbox lifted-code*))))))
     bnd*))
(nc-tail prog))

(define (primitive-relop? p)
  (Bool? (Fn-ret (grift-primitive->type p))))

(define (array-ref? p) (eq? 'Array-ref p))

;; This class of effects need to have unit returned for them
(define (primitive-effect/unit-return? p)
  (define prim (grift-primitive p))
  (and (primitive-effectfull? prim)
       (or (Unit? (Fn-ret (primitive-type prim)))
           (Bot? (Fn-ret (primitive-type prim))))))

(define (primitive-bottom? p)
  (Bot? (Fn-ret (grift-primitive->type p))))

(define (primitive-int? p)
  (Int? (Fn-ret (grift-primitive->type p))))

;; This makes sure that the value #t is returned from predicates
(: nc-value-op (-> (U UIL-Prim UIL-Prim!) D1-Value* D1-Value))
(define (nc-value-op p exp*)
  (cond
    [(primitive-relop? p)
     (If (Relop p exp*) TRUE-IMDT FALSE-IMDT)]
    [(primitive-effect/unit-return? p)
     (Begin (list (Op p exp*)) UNIT-IMDT)]
    [else (Op p exp*)]))


