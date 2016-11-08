#lang typed/racket
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
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/data0.rkt"
         "../language/data1.rkt"
         "../language/data-representation.rkt"
         (submod "../language/make-begin.rkt" typed))

;; Only the pass is provided by this module
(provide normalize-context
         (all-from-out
          "../language/data0.rkt"
          "../language/data1.rkt"))

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
       (Begin (nc-effect* eff*) (nc-tail exp))]
      [(Repeat i e1 e2 a e3 e4)
       ;; This breaks SSA but we don't rely on SSA currently
       (Begin
         (list
          (Assign a (nc-value e3))
          (Repeat i (nc-value e1) (nc-value e2) #f #f
                  (Assign a (nc-value e4))))
         (Var a))]
      [(App-Code exp exp*)
       (App-Code (nc-value exp) (nc-value* exp*))]
      [(Op p (app nc-value* v*))
       (if (uil-prim-effect? p)
           (Begin (list (Op p v*)) (Quote UNIT-IMDT))
           (nc-value-op p v*))]
      [(Var i)  (Var i)]
      [(Code-Label i) (Code-Label i)]
      [(Quote k) (Quote k)]
      [(Halt) (Halt)]
      [(Success) (Success)]
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
      [(App-Code exp exp*)
       (App-Code (nc-value exp) (nc-value* exp*))]
      [(Op p (app nc-value* v*))
       (if (uil-prim-effect? p)
           (Begin (list (Op p v*)) (Quote UNIT-IMDT))
           (nc-value-op p v*))]
      [(Var i)  (Var i)]
      [(Code-Label i) (Code-Label i)]
      [(Quote k) (Quote k)]
      [(Halt) (Halt)]
      [(Assign u (app nc-value v))
       (Begin (list (Assign u v)) (Quote UNIT-IMDT))]
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
      [(App-Code exp exp*)
       (App-Code (nc-value exp) (nc-value* exp*))]
      [(Op p exp*)
       (if (uil-prim-effect? p)
           (Op p (nc-value* exp*))
           ;; evaluate values for their effects
           (make-begin (nc-effect* exp*) NO-OP))]
      [(Var i)  NO-OP]
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
         (Relop '= (Quote TRUE-IMDT) (Var a)))]          
      [(Assign u e) (error 'nc-pred/unsuported/assign)]
      [(Op p (app nc-value* val*))
       (if (IntxInt->Bool-primitive? p)
           (match val*
             [(list a b) (Relop p a b)]
             [other (error 'nc-pred-op)])
           (Relop '= (Quote TRUE-IMDT) (nc-value-op p val*)))]
      [(app nc-value v) (Relop '= (Quote TRUE-IMDT) v)]
      [(No-Op) (error 'nc-pred "no-op in pred context")]))
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

;; This makes sure that the value #t is returned from predicates
(: nc-value-op (-> (U UIL-Prim UIL-Prim!) D1-Value* D1-Value))
(define (nc-value-op p exp*)
  (cond
    [(uil-prim-pred? p)
     (match exp*
       [(list a b)
        (If (Relop p a b) (Quote TRUE-IMDT) (Quote FALSE-IMDT)) ]
       [otherwise (error 'nc-expr-op "Unmatched ~a" exp)])]
   [(uil-prim-value? p) (Op p exp*)]
   [(uil-prim-effect? p) (Begin (list (Op p exp*)) (Quote UNIT-IMDT))]
   [else (error 'nc-value-op "primitive out of context ~v ~v" p exp*)]))
