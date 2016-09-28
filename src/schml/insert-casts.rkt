#lang typed/racket/base
#|------------------------------------------------------------------------------+
|Pass: src/insert-casts                                                         |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Description: This pass converts differences in type equality via a inserted    |
|cast. After this pass all types should be explicit and the program should be   |
|roughly equivalent to the cast-calculus mentioned in papers on the gradually   |
|typed lambda calculus.                                                         |
+-------------------------------------------------------------------------------+
|Input Grammar                                                                  |
+------------------------------------------------------------------------------|#
(require racket/match
         "../helpers.rkt"
         "../errors.rkt"
         "../configuration.rkt"
         "../language/schml1.rkt"
         "../language/cast0.rkt")

(provide insert-casts
         (all-from-out
          "../language/schml1.rkt"
          "../language/cast0.rkt"))

(: insert-casts (Schml1-Lang Config -> Cast0-Lang))
(define (insert-casts prgm comp-config)
  (match-let ([(Prog (list name next type) exp) prgm])
    (let* ([next : (Boxof Nat) (box next)]
           [exp  : C0-Expr (ic-expr next exp)]
           [next : Nat (unbox next)])
      (Prog (list name next type) exp))))

(: mk-cast ((-> String) C0-Expr Schml-Type Schml-Type -> C0-Expr))
(define (mk-cast l-th e t1 t2)
  (if (equal? t1 t2) e (Cast e (Twosome t1 t2 (l-th)))))

(: ic-expr ((Boxof Nat) S1-Expr -> C0-Expr))
(define (ic-expr next exp^)
  ;; create a new unique identifier
  (: next-uid! (String -> Uid))
  (define (next-uid! s)
    (let ([n (unbox next)])
      (set-box! next (+ n 1))
      (Uid s n)))
  ;; Recur over the ast
  (: recur (S1-Expr -> C0-Expr))
  (define (recur exp^)
    (match-let ([(Ann exp (cons src type)) exp^])
      (match exp
        [(Lambda fml* (and (Ann _ (cons body-src body-type))
                           (app recur body)))
         (unless (Fn? type) (TODO error really big here))
         (let* ([lbl-th (mk-label "lambda" body-src)]
                [body (mk-cast lbl-th body body-type (ann (Fn-ret type) Schml-Type))]
                [fml* : Uid* (map (inst Fml-identifier Uid Schml-Type) fml*)])
           (Lambda fml* body))]
        [(Let bnd* (and (Ann _ (cons src type^)) body))
         (Let (map ic-bnd bnd*) (mk-cast (mk-label "let" src) (recur body) type^ type))]
        [(Letrec bnd* (and (Ann _ (cons src type^)) body))
         (Letrec (map ic-bnd bnd*)
                 (mk-cast (mk-label "letrec" src) (recur body) type^ type))]
        [(App rator rand*) (ic-application rator rand* src type)]
        [(Op (Ann prim rand-ty*) rand*)
         (Op prim (map (ic-operand/cast src) rand* rand-ty*))]
        [(Ascribe exp t1 lbl?)
         (let ([lbl (if lbl? (th lbl?) (mk-label "ascription" src))])
           (mk-cast lbl (recur exp) t1 type))]
        [(If (and (Ann _ (cons tst-src tst-ty)) (app recur tst))
             (and (Ann _ (cons csq-src csq-ty)) (app recur csq))
             (and (Ann _ (cons alt-src alt-ty)) (app recur alt)))
         (If (mk-cast (mk-label "if" tst-src) tst tst-ty BOOL-TYPE)
             (mk-cast (mk-label "if" csq-src) csq csq-ty type)
             (mk-cast (mk-label "if" alt-src) alt alt-ty type))]
        [(Var id) (Var id)]
        [(Quote lit) (Quote lit)]
        [(Begin e* e) (Begin (map recur e*) (recur e))]
        [(Repeat id (and (Ann _ (cons s1 t1)) (app recur e1))
                 (and (Ann _ (cons s2 t2)) (app recur e2))
                 (app recur e3))
         (Repeat id
                 (mk-cast (mk-label "Repeat" s1) e1 t1 INT-TYPE)
                 (mk-cast (mk-label "Repeat" s2) e2 t2 INT-TYPE)
                 e3)]
        ;; These rules are a rough translation of the coercion semantics that
        ;; are presented in space-efficient gradual typing figure 4
        ;; Their system was using lazy-ud and this system is lazy-d
        [(Gbox e) (Gbox (recur e))]
        [(Gunbox (and (Ann _ (cons e-src e-ty)) (app recur e)))
         (cond
           [(GRef? e-ty) (Gunbox e)]
           [(Dyn? e-ty)
            (Gunbox (mk-cast (mk-label "gunbox" e-src) e e-ty (GRef DYN-TYPE)))]
           [else (TODO come up with an error)])]
        [(Gbox-set! (and (Ann _ (cons e1-src e1-ty)) (app recur e1))
                    (and (Ann _ (cons e2-src e2-ty)) (app recur e2)))
         (let ([lbl1 (mk-label "gbox-set!" e1-src)]
               [lbl2 (mk-label "gbox-set!" e2-src)])
           (cond
             [(GRef? e1-ty)
              (Gbox-set! e1 (mk-cast lbl2 e2 e2-ty (GRef-arg e1-ty)))]
             [(Dyn? e1-ty)
              (Gbox-set! (mk-cast lbl1 e1 DYN-TYPE REF-DYN-TYPE)
                         (mk-cast lbl2 e2 e2-ty DYN-TYPE))]
             [else (TODO error message that is appropriate)]))]
        [(Mbox (app recur e) t) (Mbox e t)]
        [(Munbox (app recur e)) (Munbox e)]
        [(Mbox-set! (app recur e1) (app recur e2)) (Mbox-set! e1 e2)]
        [(MunboxT (and (Ann _ (cons e-src e-ty)) (app recur e)) t)
         (cond
           [(Dyn? e-ty)
            (let ([addr (next-uid! "addr")])
              (Let `((,addr
                      .
                      ,(mk-cast (mk-label "munbox" e-src) e e-ty (MRef DYN-TYPE))))
                   (MBoxCastedRef addr t)))]
           [(MRef? e-ty) (match e
                           [(Var addr) (MBoxCastedRef addr t)]
                           [else (let ([addr (next-uid! "addr")])
                                   (Let `((,addr . ,e))
                                        (MBoxCastedRef addr t)))])]
           [else (TODO come up with an error)])]
        [(Mbox-set!T (and (Ann _ (cons e1-src e1-ty)) (app recur e1))
                     (and (Ann _ (cons e2-src e2-ty)) (app recur e2))
                     t)
         (cond
           [(Dyn? e1-ty)
            (let ([addr (next-uid! "addr")])
              (Let `((,addr
                      .
                      ,(mk-cast (mk-label "mbox-set" e1-src) e1 e1-ty (MRef DYN-TYPE))))
                   (MBoxCastedSet! addr e2 t)))]
           [(MRef? e1-ty) (match e1
                            [(Var addr) (MBoxCastedSet! addr e2 t)]
                            [else (let ([addr (next-uid! "mboxaddr")])
                                    (Let `((,addr . ,e1))
                                         (MBoxCastedSet! addr e2 t)))])]
           [else (TODO come up with an error)])]
        [(Gvector (and (Ann _ (cons size-src size-ty)) (app recur size)) (app recur e))
         (cond
           [(Dyn? size-ty)
            (Gvector (mk-cast (mk-label "gvector index" size-src) size size-ty INT-TYPE) e)]
           [else (Gvector size e)])]
        [(Gvector-ref (and (Ann _ (cons e-src e-ty)) (app recur e))
                      (and (Ann _ (cons i-src i-ty)) (app recur i)))
         (let ([e (if (Dyn? e-ty)
                      (mk-cast (mk-label "gvector-ref" e-src) e e-ty (GVect DYN-TYPE))
                      e)]
               [i (if (Dyn? i-ty)
                      (mk-cast (mk-label "gvector-ref index" i-src) i i-ty INT-TYPE)
                      i)])
           (Gvector-ref e i))]
        [(Gvector-set! (and (Ann _ (cons e1-src e1-ty)) (app recur e1))
                       (and (Ann _ (cons i-src i-ty)) (app recur i))
                       (and (Ann _ (cons e2-src e2-ty)) (app recur e2)))
         (let ([lbl1 (mk-label "gvector-set!" e1-src)]
               [lbl2 (mk-label "gvector-set!" e2-src)]
               [i (if (Dyn? i-ty)
                      (mk-cast (mk-label "gvector-ref index" i-src) i i-ty INT-TYPE)
                      i)])
           (cond
             [(GVect? e1-ty)
              (Gvector-set! e1 i (mk-cast lbl2 e2 e2-ty (GVect-arg e1-ty)))]
             [(Dyn? e1-ty)
              (Gvector-set! (mk-cast lbl1 e1 DYN-TYPE (GVect DYN-TYPE))
                            i
                            (mk-cast lbl2 e2 e2-ty DYN-TYPE))]
             [else (TODO error message that is appropriate)]))]
        [(Mvector (app recur e1) (app recur e2) t) (Mvector e1 e2 t)]
        [(Mvector-ref (app recur e1) (app recur e2))
         (Mvector-ref e1 e2)]
        [(Mvector-set! (app recur e1) (app recur e2) (app recur e3))
         (Mvector-set! e1 e2 e3)]
        [(Mvector-refT (and (Ann _ (cons e-src e-ty)) (app recur e))
                       (and (Ann _ (cons i-src i-ty)) (app recur i))
                       t)
         (let ([i (if (Dyn? i-ty)
                      (mk-cast (mk-label "mvector-ref index" i-src) i i-ty INT-TYPE)
                      i)])
           (cond
             [(Dyn? e-ty)
              (let ([addr (next-uid! "addr")])
                (Let `((,addr
                        .
                        ,(mk-cast (mk-label "munbox" e-src) e e-ty (MVect DYN-TYPE))))
                     (MVectCastedRef addr i t)))]
             [(MVect? e-ty) (match e
                              [(Var addr) (MVectCastedRef addr i t)]
                              [else (let ([addr (next-uid! "addr")])
                                      (Let `((,addr . ,e))
                                           (MVectCastedRef addr i t)))])]
             [else (TODO come up with an error)]))]
        [(Mvector-set!T (and (Ann _ (cons e1-src e1-ty)) (app recur e1))
                        (and (Ann _ (cons i-src i-ty)) (app recur i))
                        (and (Ann _ (cons e2-src e2-ty)) (app recur e2))
                        t)
         (let ([i (if (Dyn? i-ty)
                      (mk-cast (mk-label "mvector-ref index" i-src) i i-ty INT-TYPE)
                      i)])
           (cond
             [(Dyn? e1-ty)
              (let ([addr (next-uid! "addr")])
                (Let `((,addr
                        .
                        ,(mk-cast (mk-label "mbox-set" e1-src) e1 e1-ty (MRef DYN-TYPE))))
                     (MVectCastedSet! addr i e2 t)))]
             [(MVect? e1-ty) (match e1
                              [(Var addr) (MVectCastedSet! addr i e2 t)]
                              [else (let ([addr (next-uid! "mboxaddr")])
                                      (Let `((,addr . ,e1))
                                           (MVectCastedSet! addr i e2 t)))])]
             [else (TODO come up with an error)]))])))

  (: ic-bnd (S1-Bnd -> C0-Bnd))
  (define (ic-bnd b)
    (match-let ([(Bnd i t (and rhs (Ann _ (cons rhs-src rhs-type)))) b])
      (cons i (mk-cast (mk-label "binding" rhs-src) (recur rhs) rhs-type t))))


  ;; An Application of dynamic casts the arguments to dyn and the operand to
  ;; a function that takes dynamic values and returns a dynamic value
  (: ic-application (S1-Expr (Listof S1-Expr) Src Schml-Type -> C0-Expr))
  (define (ic-application rator rand* src type)
    (match-let ([(Ann _ (cons rator-src rator-type)) rator])
      (cond
        [(Dyn? rator-type)
         (let*-values ([(exp* ty*) (ic-operands rand*)]
                       [(needed-rator-type) (Fn (length ty*) ty* DYN-TYPE)]
                       [(exp)          (recur rator)]
                       [(lbl) (mk-label "Application" src rator-src)])
           (App (mk-cast lbl exp DYN-TYPE needed-rator-type) exp*))]
        ;; This is what this code needs to do. I will talk to michael about this
        ;; and see that this is really needed.
        #|
        [(Dyn? rator-type)
        (let ([rator-type (make-dyn-fn-type (length rand*))])
        (App (mk-cast (mk-label "Application" src rator-src)
        (recur rator)
        rator-type
        (make-dyn-fn-type (length rand*)))
        (map (ic-operand/cast src) rand* (Fn-fmls rator-type))))]
        |#
        [(Fn? rator-type)
         ;(Ann type)
         (App (recur rator)
              (map (ic-operand/cast src) rand* (Fn-fmls rator-type)))]
        [else (raise-pass-exn "insert-casts"
                              "error in ic-application's implimentation")])))

  (: make-dyn-fn-type (Index -> (Fn Index (Listof Schml-Type) Schml-Type)))
  (define (make-dyn-fn-type n)
    (Fn n (build-list n (lambda (_) : Schml-Type DYN-TYPE)) DYN-TYPE))

  (: ic-operands
     ((Listof S1-Expr) ->
      (values (Listof C0-Expr) (Listof Schml-Type))))
  (define (ic-operands rand*)
    (for/lists ([cf* : (Listof C0-Expr)]
                [ty* : Schml-Type*])
               ([rand : S1-Expr rand*])
      (match-let ([(Ann _ (cons _ type)) rand])
        (values (recur rand) type))))

  (: ic-operand/cast (Src -> (S1-Expr Schml-Type -> C0-Expr)))
  (define (ic-operand/cast app-src)
    (lambda (rand arg-type)
      (match-let ([(Ann _ (cons rand-src rand-type)) rand])
        (mk-cast (mk-label "application" app-src rand-src)
                 (recur rand)
                 rand-type arg-type))))

  ;; body of ic-expr
  (recur exp^))

(define-syntax-rule (th o ...)
  (lambda () o ...))

(define-syntax mk-label
  (syntax-rules ()
    [(_ pos src)
     (lambda ()
       (format "Implicit cast in ~a on expression at ~a"
               pos (srcloc->string src)))]
    [(_ pos sup-src sub-src)
     (mk-label (format "~a at ~a" pos (srcloc->string sup-src))
               sub-src)]))
