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
(require grift/src/helpers
         grift/src/errors
	 grift/src/language)

;; Only the pass is provided by this module
(provide name-intermediates)

(: name-intermediates (-> Data1-Lang Config Data2-Lang))
(trace-define (name-intermediates prgm comp-config)
  (match-let ([(Prog (list name count type) exp) (Labels bnd* tail)])
    (let*-values ([(tail count) (run-state (ni-tail tail) count)]
                  [(bnd* count) (run-state (map-state ni-bnd-code bnd*) count)])
      (Prog (list name count type)
       (Labels bnd* tail)))))

(: build-let* ((Listof (Pair D2-Bnd* D2-Bnd)) D1-Tail -> D2-Tail))
(define (build-let* b*.b t)
  (match b*.b
    ['() t]
    [(cons (cons '() bnd) b*.b)
     (Let bnd (build-let* b*.b t))]
    [(cons (cons bnd* bnd) b*.b)
     (Let bnd* (Let (list bnd) (build-let* b*.b t)))]))

(: nc-tail (-> D1-Expr (State Nat D2-Tail)))
(define (nc-tail t)
  (match t
    [(Let b* t)
     (do (bind-state : (State Nat D2-Tail))
         (b*.b : (Listof (Pair D2-Eff* D2-Bnd* D2-Bnd)) <- (map-state ni-bnd b*))
         (t    : D2-Tail <- (nc-tail t))
         (return-state (build-let* b*.b t)))]
    [(If t c a)
     (do (bind-state : (State Nat D2-Tail))
         (t : D1-Pred <- (nc-pred t))
         (c : D1-Tail <- (nc-tail c))
         (a : D1-Tail <- (nc-tail a))
         (return-state (If t c a)))]
    [(Begin eff* exp)
     (do (bind-state : (State Nat D2-Tail))
         (eff* : D2-Effect* <- (nc-effect* eff*))
         (tail : D2-Tail    <- (nc-tail exp))
         (return-state (Begin eff* tail)))]
    [other 
     (do (bind-state : (State Nat D2-Tail))
         (val : D2-Value <- (nc-value other))
         (return-state (Return val)))]))

(: nc-value (-> D1-Value (State Nat D1-Expr)))
(define (nc-value val)
  (match val
    [(Let bnd* exp)
     (do (bind-state : (State D1-Bnd-Code* D1-Value))
         (bnd* : D1-Bnd* <- (nc-bnd* bnd*))
         (val  : D1-Value <- (nc-value exp))
         (return-state (Let bnd* val)))]
    [(If t c a)
     (do (bind-state : (State D1-Bnd-Code* D1-Value))
         (t : D1-Pred  <- (nc-pred t))
         (c : D1-Value <- (nc-value c))
         (a : D1-Value <- (nc-value a))
         (return-state (If t c a)))]
    [(Begin eff* exp)
     (do (bind-state : (State D1-Bnd-Code* D1-Value))
         (eff* : D1-Effect* <- (nc-effect* eff*))
         (val  : D1-Value   <- (nc-value exp))
         (return-state (Begin eff* val)))]
    [(Repeat i st sp e)
     (do (bind-state : (State Casters D1-Expr))
         (e : D1-Effect <- (nc-effect e1))
         (return-state (Begin (list (Repeat i st sp e)) (Quote '()))))]
    [(App exp exp*)
     (do (bind-state : (State D1-Bnd-Code* D1-Value))
         (val  : D1-Value  <- (nc-value  exp))
         (val* : D1-Value* <- (nc-value* exp*))
         (return-state (App val val*)))]
    [(Op p exp*)
     ;; Filter effects into errors until I can fix this
     (do (bind-state : (State D1-Bnd-Code* D1-Value))
         (val* : D1-Value* <- (nc-value* exp*))
         (if (uil-prim-effect? p)
             (TODO do something appropriate here)
             (return-state (nc-value-op p val*))))]
    [(Halt) (return-state (Halt))]
    [(Var i) (return-state (Var i))]
    [(Code-Label i) (return-state (Code-Label i))]
    [(Quote k) (return-state (Quote k))]))

(: nc-effect (-> D0-Expr (State D1-Bnd-Code* D1-Effect)))
(define (nc-effect exp)
  (logging nc-effect ('Vomit) "~v" exp)
  (match exp
    [(Labels bnd* exp)
     (bind-state (nc-bnd-code* bnd*) (lambda (_) (nc-effect exp)))]
    [(Let bnd* exp)
     (do (bind-state : (State D1-Bnd-Code* D1-Effect))
         (bnd* : D1-Bnd* <- (nc-bnd* bnd*))
         (eff  : D1-Effect <- (nc-effect exp))
         (return-state (Let bnd* eff)))]
    [(If t c a)
     (do (bind-state : (State D1-Bnd-Code* D1-Effect))
         (t : D1-Pred  <- (nc-pred t))
         (c : D1-Effect <- (nc-effect c))
         (a : D1-Effect <- (nc-effect a))
         (return-state (If t c a)))]
    [(Begin eff* eff)
     (do (bind-state : (State D1-Bnd-Code* D1-Effect))
         (eff* : D1-Effect* <- (nc-effect* eff*))
         (eff  : D1-Effect  <- (nc-effect eff))
         (return-state (make-begin (append eff* (list eff)) NO-OP)))]
    [(Repeat i st sp e)
     (do (bind-state : (State Casters D1-Expr))
         (e : D1-Effect <- (nc-effect e))
         (return-state (Repeat i st sp e3)))]
    [(App exp exp*)
     (do (bind-state : (State D1-Bnd-Code* D1-Effect))
         (val  : D1-Value  <- (nc-value  exp))
         (val* : D1-Value* <- (nc-value* exp*))
         (return-state (App val val*)))]
    [(Op p exp*)
     (if (uil-prim-effect? p)
         ;; effects need to remain
         (do (bind-state : (State D1-Bnd-Code* D1-Effect))
             (val* : D1-Value* <- (nc-value* exp*))
             (return-state (Op p val*)))
         ;; values are evaluated for their effect
         (do (bind-state : (State D1-Bnd-Code* D1-Effect))
             (eff* : D1-Effect* <- (nc-effect* exp*))
             (return-state (make-begin eff* NO-OP))))]
    ;; I am not sure where to put this
    [(Halt) (TODO halt should always be the logical conclusion)]
    [(Var i) (return-state NO-OP)]
    [(Code-Label i) (return-state NO-OP)]
    [(Quote k) (return-state NO-OP)]))

(: nc-pred (-> D0-Expr (State D1-Bnd-Code* D1-Pred)))
(define (nc-pred exp)
  (logging nc-pref ('Vomit) "~v" exp)
  (match exp
    [(Labels bnd* exp)
     (bind-state (nc-bnd-code* bnd*) (lambda (_) (nc-pred exp)))]
    [(Let bnd* exp)
     (do (bind-state : (State D1-Bnd-Code* D1-Pred))
         (bnd* : D1-Bnd*  <- (nc-bnd* bnd*))
         (val  : D1-Pred <- (nc-pred exp))
         (return-state (Let bnd* val)))]
    [(If t c a)
     (do (bind-state : (State D1-Bnd-Code* D1-Pred))
         (t : D1-Pred  <- (nc-pred t))
         (c : D1-Pred <- (nc-pred c))
         (a : D1-Pred <- (nc-pred a))
         (return-state (If t c a)))]
    [(Begin eff* exp)
     (do (bind-state : (State D1-Bnd-Code* D1-Pred))
         (eff* : D1-Effect* <- (nc-effect* eff*))
         (val  : D1-Pred   <- (nc-pred exp))
         (return-state (Begin eff* val)))]
    [(App exp exp*)
     (do (bind-state : (State D1-Bnd-Code* D1-Pred))
         (val  : D1-Value  <- (nc-value  exp))
         (val* : D1-Value* <- (nc-value* exp*))
         (return-state (App val val*)))]
    [(Op p exp*)
     (do (bind-state : (State D1-Bnd-Code* D1-Pred))
         ;; for some reason I am enforcing this wierd constrain about
         ;; ops in pred position. This is probably foolish we should try
         ;; to eliminate this.
         (val* : D1-Value* <- (nc-value* exp*))
         (if (IntxInt->Bool-primitive? p)
             (match val*
               [(list a b) (return-state (Relop p a b))]
               [other (error 'nc-pred-op)])
             (return-state (Relop '= (Quote TRUE-IMDT) (nc-value-op p val*)))))]
    [other
     (do (bind-state : (State D1-Bnd-Code* D1-Pred))
         (v : D1-Value <- (nc-value exp))
         (return-state (Relop '= (Quote TRUE-IMDT) v)))]))

;; This makes sure that the value #t is returned from predicates
(: nc-value-op (-> (U UIL-Prim UIL-Prim!) D1-Value* D1-Value))
(define (nc-value-op p exp*)
  (cond
   [(IntxInt->Bool-primitive? p)
    (match exp*
      [(list a b)
       (If (Relop p a b) (Quote TRUE-IMDT) (Quote FALSE-IMDT))]
      [otherwise (error 'nc-expr-op "Unmatched ~a" exp)])]
   [(uil-prim-value? p) (Op p exp*)]
   [else (error 'nc-value-op "primitive out of context")]))

(: nc-value* (-> (Listof D0-Expr) (State D1-Bnd-Code* D1-Value*)))
(define (nc-value* exp*) (map-state nc-value exp*))
(: nc-effect* (-> (Listof D0-Expr) (State D1-Bnd-Code* D1-Effect*)))
(define (nc-effect* exp*) (map-state nc-effect exp*))

(: nc-bnd (-> D0-Bnd (State D1-Bnd-Code* D1-Bnd)))
(define (nc-bnd bnd)
  (match-let ([(cons uid rhs) bnd])
    (do (bind-state : (State D1-Bnd-Code* D1-Bnd))
        (value : D1-Value <- (nc-value rhs))
        (return-state (ann (cons uid value) D1-Bnd)))))

(: nc-bnd* (-> D0-Bnd* (State D1-Bnd-Code* D1-Bnd*)))
(define (nc-bnd* bnd*) (map-state nc-bnd bnd*))

(: nc-bnd-code* (-> D0-Bnd-Code* (State D1-Bnd-Code* Null)))
(define (nc-bnd-code* bnd*) (foldr-state nc-bnd-code '() bnd*))

(: nc-bnd-code (-> D0-Bnd-Code Null (State D1-Bnd-Code* Null)))
(define (nc-bnd-code b nil)
  (do (bind-state : (State D1-Bnd-Code* Null))
      (match-let ([(cons u (Code u* e)) b])
        (t : D1-Tail <- (nc-tail e))
        (let ([b : D1-Bnd-Code (cons u (Code u* t))])
          (b* : D1-Bnd-Code* <- get-state)
          (put-state (cons b b*))))))
