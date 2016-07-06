#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/insert-casts                                                         |
+-------------------------------------------------------------------------------+
|Author: Deyaaeldeen Almahallawi (dalmahal@indiana.edu)                         |
+-------------------------------------------------------------------------------+
|Description: This pass rewrites letrec expressions so that, in the output of   |
| the pass, all letrec expressions are "pure," i.e., bind only variables to     |
| lambda expressions.                                                           |
+-------------------------------------------------------------------------------+
|Input Grammar: Cast0-Lang                                                      |
+------------------------------------------------------------------------------|#
(require "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/cast0.rkt"
         "../language/cast-with-pure-letrec.rkt")

;; Only the pass is provided by this module
(provide
 purify-letrec
 (all-from-out
  "../language/cast0.rkt"
  "../language/cast-with-pure-letrec.rkt"))

(: purify-letrec (Cast0-Lang Config -> Cast/Pure-Letrec))
(define (purify-letrec prgm comp-config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let-values ([(exp next) (run-state (pl-expr exp) next)])
      (Prog (list name next type) exp))))

(: pl-bnd* (-> C0-Bnd* (State Nat C/PL-Bnd*)))
(define (pl-bnd* b*)
  (: pl-bnd (-> C0-Bnd (State Nat C/PL-Bnd)))
  (define (pl-bnd b)
    (match-let ([(cons i e) b])
      (do (bind-state : (State Nat C/PL-Bnd))
          (e : C/PL-Expr <- (pl-expr e))
        (return-state (cons i e)))))
  (map-state pl-bnd b*))

(: gen-uid* (Nat -> (State Nat (Listof Uid))))
(define ((gen-uid* n) next)
  (let ([f : (Nat -> Uid) (lambda (n) (Uid "purifiedbinds" n))])
    (values (map f (range next (+ next n))) (+ next n))))

(: simple?* ((Listof C0-Expr) Uid* Integer Boolean -> Boolean))
(define (simple?* exp uid* depth outer-lambda?)
  (if (simple? (car exp) uid* depth outer-lambda?)
      (simple?* (cdr exp) uid* depth outer-lambda?)
      #f))

;; TODO: is it possible to merge simple? and pl-expr to make only one
;; pass over the AST?
(: simple? (C0-Expr Uid* Integer Boolean -> Boolean))
(define (simple? exp uid* depth outer-lambda?)
  (match exp
    [(Quote _) #t]
    [(Var x) (if (memq x uid*) #t #f)]
    [(Lambda _ e)
     (if (> depth 0)
         (simple? e uid* (+ 1 depth) #t)
         #f)]
    [(Letrec bnd* exp)
     (and (simple? exp uid* (+ 1 depth) outer-lambda?)
          (simple?* (map (inst cdr Uid C0-Expr) bnd*) uid* (+ 1 depth) outer-lambda?))]
    [(Let bnd* exp)
     (and (simple? exp uid* (+ 1 depth) outer-lambda?)
          (simple?* (map (inst cdr Uid C0-Expr) bnd*) uid* (+ 1 depth) outer-lambda?))]
    [(App e e*)
     (if outer-lambda?
         (and (simple?* e* uid* (+ 1 depth) outer-lambda?)
              (simple? e uid* (+ 1 depth) outer-lambda?))
         #f)]
    [(Op _ e*) (simple?* e* uid* (+ 1 depth) outer-lambda?)]
    [(If e1 e2 e3)
     (and (simple? e1 uid* (+ 1 depth) outer-lambda?)
          (simple? e2 uid* (+ 1 depth) outer-lambda?)
          (simple? e3 uid* (+ 1 depth) outer-lambda?))]
    [(Cast e r) (simple? e uid* (+ 1 depth) outer-lambda?)]
    [(Begin e* e)
     (and (simple? e uid* (+ 1 depth) outer-lambda?)
          (simple?* e* uid* (+ 1 depth) outer-lambda?))]
    [(Repeat i e1 e2 e3)
     (and (simple? e1 uid* (+ 1 depth) outer-lambda?)
          (simple? e2 uid* (+ 1 depth) outer-lambda?)
          (simple? e3 uid* (+ 1 depth) outer-lambda?))]
    [(Gbox e) (simple? e uid* (+ 1 depth) outer-lambda?)]
    [(Gunbox e) (simple? e uid* (+ 1 depth) outer-lambda?)]
    [(Gbox-set! e1 e2)
     (and (simple? e1 uid* (+ 1 depth) outer-lambda?)
          (simple? e2 uid* (+ 1 depth) outer-lambda?))]
    [(Gvector e1 e2)
     (and (simple? e1 uid* (+ 1 depth) outer-lambda?)
          (simple? e2 uid* (+ 1 depth) outer-lambda?))]
    [(Gvector-set! e1 e2 e3)
     (and (simple? e1 uid* (+ 1 depth) outer-lambda?)
          (simple? e2 uid* (+ 1 depth) outer-lambda?)
          (simple? e3 uid* (+ 1 depth) outer-lambda?))]
    [(Gvector-ref e1 e2)
     (and (simple? e1 uid* (+ 1 depth) outer-lambda?)
          (simple? e2 uid* (+ 1 depth) outer-lambda?))]
    [(Create-tuple e*)
     (simple?* e* uid* (+ 1 depth) outer-lambda?)]
    [(Tuple-proj e _)
     (simple? e uid* (+ 1 depth) outer-lambda?)]))

;; Computes the intersection between two lists and the list of elements
;; that is in the first list but not in the second.
(: diff1-intersect (C/PL-Bnd-Lam* Uid* -> (Values C/PL-Bnd-Lam* C/PL-Bnd-Lam*)))
(define (diff1-intersect l1 l2)
  (for/fold ([no* : C/PL-Bnd-Lam* '()] [yes* : C/PL-Bnd-Lam* '()])
            ([a : C/PL-Bnd-Lam l1])
    (match-let ([(cons i e) a])
      (if (memq i l2)
          (values no* (cons a yes*))
          (values (cons a no*) yes*)))))

;; Computes the list of elements in the second list but not in the first.
(: diff2 (Uid* Uid* -> Uid*))
(define (diff2 l1 l2)
  (let-values ([(l) (for/fold ([no* : Uid* '()])
                              ([a : Uid l2])
                      (if (memq a l1)
                          (values no*)
                          (values (cons a no*))))])
    l))

(: replace-ref-lam (C/PL-Lam (Listof Uid) -> C/PL-Lam))
(define (replace-ref-lam exp v*)
  (match-let ([(Lambda f* e) exp])
    (Lambda f* (replace-ref e v*))))

(: replace-ref (C/PL-Expr (Listof Uid) -> C/PL-Expr))
(define (replace-ref exp v*)
  (let ([recur/env : (C/PL-Expr -> C/PL-Expr) (lambda (e) (replace-ref e v*))])
    (match exp
      [(Quote n) exp]
      [(Var x)
       (if (memq x v*)
           (Gunbox (Var x))
           (Var x))]
      [(Lambda f* e) (Lambda f* (replace-ref e v*))]
      [(Letrec bnd* e)
       (let-values ([(no* yes*) (diff1-intersect bnd* v*)])
         (let* ([nob* (map (inst car Uid C/PL-Lam) no*)]
                [no* (map (lambda ([e : C/PL-Lam]) : C/PL-Lam
                                  (replace-ref-lam e v*))
                          (map (inst cdr Uid C/PL-Lam) no*))]
                [no* (map (inst cons Uid C/PL-Lam) nob* no*)]
                [v* (diff2 (map (inst car Uid C/PL-Lam) bnd*) v*)]
                [e (if (> (length v*) 0) (replace-ref e v*) e)])
           (Letrec (append no* yes*) e)))]
      [(Let bnd* e)
       (let* ([b* (map (inst car Uid C/PL-Expr) bnd*)]
              [bnd* (map (inst cons Uid C/PL-Expr) b* (map recur/env (map (inst cdr Uid C/PL-Expr) bnd*)))]
              [v* (diff2 b* v*)]
              [e (if (> (length v*) 0) (replace-ref e v*) e)])
         (Let bnd* e))]
      [(App e e*)
       (App (replace-ref e v*) (map recur/env e*))]
      [(Op op e*) (Op op (map recur/env e*))]
      [(If e1 e2 e3)
       (If (replace-ref e1 v*) (replace-ref e2 v*) (replace-ref e3 v*))]
      [(Cast e r)
       (match r
         [(Twosome t1 t2 lbl)
          (Cast (replace-ref e v*) (Twosome t1 t2 lbl))]
         [(Coercion c)
          (Cast (replace-ref e v*) (Coercion c))])]
      [(Begin e* e)
       (Begin (map recur/env e*) (replace-ref e v*))]
      [(Repeat i e1 e2 e3)
       (Repeat i (replace-ref e1 v*) (replace-ref e2 v*) (replace-ref e3 v*))]
      [(Gbox e) (Gbox (replace-ref e v*))]
      [(Gunbox e) (Gunbox (replace-ref e v*))]
      [(Gbox-set! e1 e2)
       (Gbox-set! (replace-ref e1 v*) (replace-ref e2 v*))]
      [(Gvector e1 e2)
       (Gvector (replace-ref e1 v*) (replace-ref e2 v*))]
      [(Gvector-set! e1 e2 e3)
       (Gvector-set! (replace-ref e1 v*) (replace-ref e2 v*) (replace-ref e3 v*))]
      [(Gvector-ref e1 e2)
       (Gvector-ref (replace-ref e1 v*) (replace-ref e2 v*))]
      [(Create-tuple e*) (Create-tuple (map recur/env e*))]
      [(Tuple-proj e i) (Tuple-proj (replace-ref e v*) i)])))

(: pl-expr (C0-Expr -> (State Nat C/PL-Expr)))
(define (pl-expr exp)
  (do (bind-state : (State Nat C/PL-Expr))
      (match exp
        [(Lambda f* exp)
         (exp : C/PL-Expr <- (pl-expr exp))
         (return-state (Lambda f* exp))]
        [(Cast exp r)
         (exp : C/PL-Expr <- (pl-expr exp))
         (match r
           [(Twosome t1 t2 lbl)
            (return-state (Cast exp (Twosome t1 t2 lbl)))]
           [(Coercion c)
            (return-state (Cast exp (Coercion c)))])]
        [(Letrec bnd* exp)
         (exp : C/PL-Expr  <- (pl-expr exp))
         (next : Nat <- get-state)
         (let-values ([(simple* complex* lambda* next)
                       (for/fold ([s* : C/PL-Bnd* '()] [c* : C/PL-Bnd* '()] [l* : C/PL-Bnd-Lam* '()] [n : Nat next])
                                 ([bnd : C0-Bnd bnd*])
                         (match-let ([(cons i exp) bnd])
                           (match exp
                             [(Lambda f* exp)
                              (let-values ([(exp n) (run-state (pl-expr exp) n)])
                                (values s* c* (cons (cons i (Lambda f* exp)) l*) n))]
                             [exp
                              (let-values ([(e n) (run-state (pl-expr exp) n)])
                                (if (simple? exp (map (inst car Uid C0-Expr) bnd*) 0 #f)
                                    (values (cons (cons i e) s*) c* l* n)
                                    (values s* (cons (cons i e) c*) l* n)))])))])
           ((put-state next) : (State Nat Null))
           (t* : (Listof Uid) <- (gen-uid* (length complex*)))
           (let* ([c* (map (inst car Uid C/PL-Expr) complex*)]
                  [exp (if (> (length c*) 0) (replace-ref exp c*) exp)]
                  [complex* (if (> (length c*) 0)
                                (map (lambda ([e : C/PL-Bnd]) : C/PL-Bnd
                                             (match-let ([(cons i e) e])
                                               (cons i (replace-ref e c*)))) complex*)
                                complex*)]
                  [lambda* (if (> (length c*) 0)
                               (map (lambda ([e : C/PL-Bnd-Lam]) : C/PL-Bnd-Lam
                                            (match-let ([(cons i e) e])
                                              (cons i (replace-ref-lam e c*)))) lambda*)
                               lambda*)]
                  [f : (Uid Uid -> C/PL-Expr) (lambda (c t) (Gbox-set! (Var c) (Var t)))]
                  [let-cbnd* (map (inst cons Uid C/PL-Expr)
                                  c*
                                  (make-list (length c*) (Gbox (Quote 0))))]
                  [let-tbnd* (map (inst cons Uid C/PL-Expr) t* (map (inst cdr Uid C/PL-Expr) complex*))])
             (cond
               [(and (null? complex*) (null? lambda*) (null? simple*)) (return-state exp)]
               [(and (null? complex*) (null? simple*)) (return-state (Letrec lambda* exp))]
               [(and (null? complex*) (null? lambda*)) (return-state (Let simple* exp))]
               [(and (null? simple*) (null? lambda*))
                (return-state
                 (Let let-cbnd*
                      (Let let-tbnd* (Begin (map f c* t*) exp))))]
               [(null? lambda*)
                (return-state
                 (Let (append let-cbnd* let-tbnd*) (Begin (map f c* t*) exp)))]
               [(null? complex*)
                (return-state
                 (Let simple* (Letrec lambda* exp)))]
               [(null? simple*)
                (return-state
                 (Let let-cbnd*
                      (Letrec lambda*
                              (Let let-tbnd* (Begin (map f c* t*) exp)))))]
               [else
                (return-state
                 (Let simple*
                      (Let let-cbnd*
                           (Letrec lambda*
                                   (Let let-tbnd* (Begin (map f c* t*) exp))))))])))]
        [(Let bnd* exp)
         (bnd* : C/PL-Bnd* <- (pl-bnd* bnd*))
         (exp : C/PL-Expr  <- (pl-expr exp))
         (return-state (Let bnd* exp))]
        [(App exp exp*)
         (exp  : C/PL-Expr  <- (pl-expr  exp))
         (exp* : C/PL-Expr* <- (map-state pl-expr exp*))
         (return-state (App exp exp*))]
        [(Op p exp*)
         (exp* : C/PL-Expr* <- (map-state pl-expr exp*))
         (return-state (Op p exp*))]
        [(If tst csq alt)
         (tst : C/PL-Expr <- (pl-expr tst))
         (csq : C/PL-Expr <- (pl-expr csq))
         (alt : C/PL-Expr <- (pl-expr alt))
         (return-state (If tst csq alt))]
        [(Begin e* e)
         (e* : C/PL-Expr* <- (map-state pl-expr e*))
         (e  : C/PL-Expr  <- (pl-expr  e))
         (return-state (Begin e* e))]
        [(Repeat i e1 e2 e3)
         (e1 : C/PL-Expr <- (pl-expr e1))
         (e2 : C/PL-Expr <- (pl-expr e2))
         (e3 : C/PL-Expr <- (pl-expr e3))
         (return-state (Repeat i e1 e2 e3))]
        [(Gbox e) (lift-state (inst Gbox C/PL-Expr) (pl-expr e))]
        [(Gunbox e) (lift-state (inst Gunbox C/PL-Expr) (pl-expr e))]
        [(Gbox-set! e1 e2) (lift-state (inst Gbox-set! C/PL-Expr C/PL-Expr)
                                       (pl-expr e1) (pl-expr e2))]
        [(Gvector n e) (lift-state (inst Gvector C/PL-Expr C/PL-Expr) (pl-expr n) (pl-expr e))]
        [(Gvector-ref e index) (lift-state (inst Gvector-ref C/PL-Expr C/PL-Expr) (pl-expr e) (pl-expr index))]
        [(Gvector-set! e1 index e2) (lift-state (inst Gvector-set! C/PL-Expr C/PL-Expr C/PL-Expr)
                                                (pl-expr e1) (pl-expr index) (pl-expr e2))]
        [(Create-tuple e*)
         (e* : C/PL-Expr* <- (map-state pl-expr e*))
         (return-state (Create-tuple e*))]
        [(Tuple-proj e i)
         (e  : C/PL-Expr <- (pl-expr e))
         (return-state (Tuple-proj e i))]
        [(Var id)    (return-state (Var id))]
        [(Quote lit) (return-state (Quote lit))])))
