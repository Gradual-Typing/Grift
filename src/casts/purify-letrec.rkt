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
    (let* ([next : (Boxof Nat) (box next)]
           [exp  : C/PL-Expr (pl-expr next exp)]
           [next : Nat (unbox next)])
      (Prog (list name next type) exp))))

(: pl-expr ((Boxof Nat) C0-Expr -> C/PL-Expr))
(define (pl-expr next exp)

  ;; create a new unique identifier
  (: next-uid! (String -> Uid))
  (define (next-uid! s)
    (let ([n (unbox next)])
      (set-box! next (+ n 1))
      (Uid s n)))
  
  (: next-uid*! (Nat -> (Listof Uid)))
  (define (next-uid*! n)
    (let ([f : (Nat -> Uid) (lambda (n) (Uid "purifiedbinds" n))]
          [unboxed (unbox next)])
      (set-box! next (+ unboxed n))
      (map f (range unboxed (+ unboxed n)))))
  
  ;; Recur over the ast
  (: recur (C0-Expr -> C/PL-Expr))
  (define (recur exp)
    (match exp
      [(Lambda f* exp) (Lambda f* (recur exp))]
      [(Cast exp r)
       (match r
         [(Twosome t1 t2 lbl) (Cast (recur exp) (Twosome t1 t2 lbl))]
         [(Coercion c) (Cast (recur exp) (Coercion c))])]
      [(Letrec bnd* exp)
       (let-values ([(simple* complex* lambda*)
                     (for/fold ([s* : C/PL-Bnd* '()] [c* : C/PL-Bnd* '()] [l* : C/PL-Bnd-Lam* '()])
                               ([bnd : C0-Bnd bnd*])
                       (match-let ([(cons i exp) bnd])
                         (match exp
                           [(Lambda f* exp)
                            (let ([exp (recur exp)])
                              (values s* c* (cons (cons i (Lambda f* exp)) l*)))]
                           [exp
                            (let ([e (recur exp)])
                              (if (simple? exp (map (inst car Uid C0-Expr) bnd*) 0 #f)
                                  (values (cons (cons i e) s*) c* l*)
                                  (values s* (cons (cons i e) c*) l*)))])))])
         (let* ([t* (next-uid*! (length complex*))]
                [c* (map (inst car Uid C/PL-Expr) complex*)]
                [exp (recur exp)]
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
             [(and (null? complex*) (null? lambda*) (null? simple*)) exp]
             [(and (null? complex*) (null? simple*)) (Letrec lambda* exp)]
             [(and (null? complex*) (null? lambda*)) (Let simple* exp)]
             [(and (null? simple*) (null? lambda*))
              (Let let-cbnd*
                   (Let let-tbnd* (Begin (map f c* t*) exp)))]
             [(null? lambda*)
              (Let (append let-cbnd* let-tbnd*) (Begin (map f c* t*) exp))]
             [(null? complex*)
              (Let simple* (Letrec lambda* exp))]
             [(null? simple*)
              (Let let-cbnd*
                   (Letrec lambda*
                           (Let let-tbnd* (Begin (map f c* t*) exp))))]
             [else
              (Let simple*
                   (Let let-cbnd*
                        (Letrec lambda*
                                (Let let-tbnd* (Begin (map f c* t*) exp)))))])))]
      [(Let bnd* exp) (Let (pl-bnd* bnd*) (recur exp))]
      [(App exp exp*) (App (recur exp) (map recur exp*))]
      [(Op p exp*) (Op p (map recur exp*))]
      [(If tst csq alt) (If (recur tst) (recur csq) (recur alt))]
      [(Begin e* e) (Begin (map recur e*) (recur e))]
      [(Repeat i e1 e2 e3) (Repeat i (recur e1) (recur e2) (recur e3))]
      [(Gbox e) (Gbox (recur e))]
      [(Gunbox e) (Gunbox (recur e))]
      [(Gbox-set! e1 e2) (Gbox-set! (recur e1) (recur e2))]
      [(Gvector n e) (Gvector (recur n) (recur e))]
      [(Gvector-ref e index) (Gvector-ref (recur e) (recur index))]
      [(Gvector-set! e1 index e2) (Gvector-set! (recur e1) (recur index) (recur e2))]
      [(Mbox e t) (Mbox (recur e) t)]
      [(Munbox e) (Munbox (recur e))]
      [(Mbox-set! e1 e2) (Mbox-set! (recur e1) (recur e2))]
      [(MBoxCastedRef u t) (MBoxCastedRef u t)]
      [(MBoxCastedSet! u e t) (MBoxCastedSet! u (recur e) t)]
      [(Mvector e1 e2 t) (Mvector (recur e1) (recur e2) t)]
      [(Mvector-ref e1 e2) (Mvector-ref (recur e1) (recur e2))]
      [(Mvector-set! e1 e2 e3)
       (Mvector-set! (recur e1) (recur e2) (recur e3))]
      [(MVectCastedRef u (app recur i) t) (MVectCastedRef u i t)]
      [(MVectCastedSet! u i e t)
       (MVectCastedSet! u (recur i) (recur e) t)]
      [(Var x) (Var x)]
      [(Quote c) (Quote c)]))

  (: pl-bnd* (C0-Bnd* -> C/PL-Bnd*))
  (define (pl-bnd* b*)
    (: pl-bnd (C0-Bnd -> C/PL-Bnd))
    (define (pl-bnd b)
      (match-let ([(cons i e) b])
        (cons i (recur e))))
    (map pl-bnd b*))

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
      [(Mbox e _) (simple? e uid* (+ 1 depth) outer-lambda?)]
      [(Munbox e) (simple? e uid* (+ 1 depth) outer-lambda?)]
      [(Mbox-set! e1 e2)
       (and (simple? e1 uid* (+ 1 depth) outer-lambda?)
            (simple? e2 uid* (+ 1 depth) outer-lambda?))]
      [(MBoxCastedRef _ _) #t]
      [(MBoxCastedSet! _ e _)
       (simple? e uid* (+ 1 depth) outer-lambda?)]
      [(Mvector e1 e2 _)
       (and (simple? e1 uid* (+ 1 depth) outer-lambda?)
            (simple? e2 uid* (+ 1 depth) outer-lambda?))]
      [(Mvector-ref e1 e2)
       (and (simple? e1 uid* (+ 1 depth) outer-lambda?)
            (simple? e2 uid* (+ 1 depth) outer-lambda?))]
      [(Mvector-set! e1 e2 e3)
       (and (simple? e1 uid* (+ 1 depth) outer-lambda?)
            (simple? e2 uid* (+ 1 depth) outer-lambda?)
            (simple? e3 uid* (+ 1 depth) outer-lambda?))]
      [(MVectCastedRef _ i _) (simple? i uid* (+ 1 depth) outer-lambda?)]
      [(MVectCastedSet! _ i e _)
       (and (simple? i uid* (+ 1 depth) outer-lambda?)
            (simple? e uid* (+ 1 depth) outer-lambda?))]))

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
        [(Mbox e t) (Mbox (replace-ref e v*) t)]
        [(Munbox e) (Munbox (replace-ref e v*))]
        [(Mbox-set! e1 e2) (Mbox-set! (replace-ref e1 v*) (replace-ref e2 v*))]
        [(MBoxCastedRef u t) (MBoxCastedRef u t)]
        [(MBoxCastedSet! u e t)
         (MBoxCastedSet! u (replace-ref e v*) t)]
        [(Mvector e1 e2 t)
         (Mvector (replace-ref e1 v*) (replace-ref e2 v*) t)]
        [(Mvector-ref e1 e2) (Mvector-ref (replace-ref e1 v*) (replace-ref e2 v*))]
        [(Mvector-set! e1 e2 e3)
         (Mvector-set! (replace-ref e1 v*) (replace-ref e2 v*) (replace-ref e3 v*))]
        [(MVectCastedRef u i t) (MVectCastedRef u (replace-ref i v*) t)]
        [(MVectCastedSet! u i e t)
         (MVectCastedSet! u (replace-ref i v*) (replace-ref e v*) t)])))

  ;; body of pl-expr
  (recur exp))
