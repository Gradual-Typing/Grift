#lang racket/base
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
         "../errors.rkt"
         "../logging.rkt"
         "../configuration.rkt"
         "../language/forms.rkt")

;; Only the pass is provided by this module
(provide insert-casts)

(module* typed typed/racket/base
  (require "../language/schml1.rkt"
           "../language/cast0.rkt")
  (provide (all-from-out
            "../language/schml1.rkt"
            "../language/cast0.rkt"))
  (require/typed/provide (submod "..")
    [insert-casts (Schml1-Lang -> Cast0-Lang)]))

(define-syntax-rule (: stx ...) (void))
(define-syntax-rule (ann e t ...) e)
(define-syntax-rule (inst e t ...) e)

;; A list of casts that are inserted and there source location
(: casts-inserted (Boxof (Listof (cons srcloc C0-Expr))))
(define casts-inserted (box '()))
(: add-cast! : srcloc C0-Expr -> Void)
(define (add-cast! s e)
  (set-box! casts-inserted (cons (cons s e) (unbox casts-inserted))))
(: no-casts! : -> Void)
(define (no-casts!)
  (set-box! casts-inserted '()))


(: insert-casts (Schml1-Lang -> Cast0-Lang))
(define (insert-casts prgm)
  (match-define (Prog (list name unique type) tl*) prgm)
  (define uc (make-unique-counter unique))
  (no-casts!)
  (define new-tl*
    (parameterize ([current-unique-counter uc])
      (for/list ([tl (in-list tl*)])
        (match tl
          [(Define r? id t2 (and (Ann _ (cons s t1)) (app ic-expr e)))
           (Define r? id t2 (mk-cast s (mk-label "Define" s) e t1 t2))]
          [(Observe e t) (Observe (ic-expr e) t)]))))
  (when (and (program-must-be-statically-typed?)
             (not (null? (unbox casts-inserted))))
    (error 'insert-casts "asserted program was static: it is not"))
  (debug (Prog (list name (unique-counter-next! uc) type) new-tl*)))

(: mk-cast ((-> Blame-Label) C0-Expr Schml-Type Schml-Type -> C0-Expr))
(define (mk-cast src l-th e t1 t2)
  (cond
    [(equal? t1 t2) e]
    [else
     (define c (debug (Cast e (Twosome t1 t2 (l-th)))))
     (add-cast! src c)
     c]))


(: ic-expr (S1-Expr . -> . C0-Expr))
(define (ic-expr exp^)
  (match-let ([(Ann exp (cons src type)) exp^])
    (match exp
      [(Lambda fml* (and (Ann _ (cons body-src body-type))
                         (app ic-expr body)))
       (unless (Fn? type)
         (error 'insert-casts "function type recieve non-function type"))
       (let* ([lbl-th (mk-label "lambda" body-src)]
              [body (mk-cast body-src lbl-th body
                             body-type (ann (Fn-ret type) Schml-Type))]
              [fml* (map (inst Fml-identifier Uid Schml-Type) fml*)])
         (Lambda fml* body))]
      [(Let bnd* (and (Ann _ (cons src type^)) body))
       (Let (map ic-bnd bnd*)
         (mk-cast src (mk-label "let" src) (ic-expr body) type^ type))]
      [(Letrec bnd* (and (Ann _ (cons src type^)) body))
       (Letrec (map ic-bnd bnd*)
         (mk-cast src (mk-label "letrec" src) (ic-expr body) type^ type))]
      [(App rator rand*)
       (ic-application rator rand* src type)]
      [(Op (Ann prim rand-ty*) rand*)
       (Op prim (map (ic-operand/cast src) rand* rand-ty*))]
      [(Ascribe exp t1 lbl?)
       (let ([lbl (if lbl? (th lbl?) (mk-label "ascription" src))])
         (mk-cast src lbl (ic-expr exp) t1 type))]
      [(If (and (Ann _ (cons tst-src tst-ty)) (app ic-expr tst))
           (and (Ann _ (cons csq-src csq-ty)) (app ic-expr csq))
           (and (Ann _ (cons alt-src alt-ty)) (app ic-expr alt)))
       (If (mk-cast tst-src (mk-label "if" tst-src) tst tst-ty BOOL-TYPE)
           (mk-cast csq-src (mk-label "if" csq-src) csq csq-ty type)
           (mk-cast alt-src (mk-label "if" alt-src) alt alt-ty type))]
      [(Switch (and (Ann _ (cons es et)) (app ic-expr e))
               c*
               (and (Ann _ (cons ds dt)) (app ic-expr d)))
       (Switch (mk-cast es (mk-label "switch" es) e et INT-TYPE)
               (for/list #;#;: (Switch-Case* C0-Expr) ([c c*])
                 (match-let ([(cons cl (and (Ann _ (cons cs ct))
                                            (app ic-expr cr))) c])
                   (cons cl (mk-cast cs (mk-label "switch" cs) cr ct type))))
               (mk-cast ds (mk-label "switch" ds) d dt type))]
      [(Var id) (Var id)]
      [(Quote lit) (Quote lit)]
      [(Begin e* e) (Begin (map ic-expr e*) (ic-expr e))]
      [(Repeat index
           (and (Ann _ (cons s1 t1)) (app ic-expr e1))
           (and (Ann _ (cons s2 t2)) (app ic-expr e2))
         acc
         (and (Ann _ (cons s3 t3)) (app ic-expr e3))
         (and (Ann _ (cons s4 t4)) (app ic-expr e4)))
       (Repeat
           index
           (mk-cast s1 (mk-label "Repeat" s1) e1 t1 INT-TYPE)
           (mk-cast s2 (mk-label "Repeat" s2) e2 t2 INT-TYPE)
         acc
         (mk-cast s3 (mk-label "Repeat" s3) e3 t3 type)
         (mk-cast s4 (mk-label "Repeat" s4) e4 t4 type))]
      ;; These rules are a rough translation of the coercion semantics that
      ;; are presented in space-efficient gradual typing figure 4
      ;; Their system was using lazy-ud and this system is lazy-d
      [(Gbox e) (Gbox (ic-expr e))]
      [(Gunbox (and (Ann _ (cons e-src e-ty)) (app ic-expr e)))
       (cond
         [(GRef? e-ty) (Gunbox e)]
         [(Dyn? e-ty)
          (define lbl (mk-label "guarded unbox" e-src))
          (cond
            [(dynamic-operations?)
             ;; TODO we should record this as being a cast
             ;; but don't have the infrastructure.
             (Dyn-GRef-Ref e (lbl))]
            [else             
             (Gunbox (mk-cast e-src lbl e e-ty (GRef DYN-TYPE)))])]
         [else
          (error 'insert-casts/gunbox
                 "unexexpected value for e-ty: ~a" e-ty)])]
      [(Gbox-set! (and (Ann _ (cons e1-src e1-ty)) (app ic-expr e1))
                  (and (Ann _ (cons e2-src e2-ty)) (app ic-expr e2)))
       (define lbl1 (mk-label "guarded box-set!" e1-src))
       (define lbl2 (mk-label "guarded box-set!" e2-src))
       (cond
         [(GRef? e1-ty)
          (Gbox-set! e1 (mk-cast e2-src lbl2 e2 e2-ty (GRef-arg e1-ty)))]
         [(Dyn? e1-ty)
          (cond
            [(dynamic-operations?)
             ;; TODO register dynamic operation as a cast
             (Dyn-GRef-Set! e1 e2 e2-ty (lbl1))]
            [else
             
             (Gbox-set! (mk-cast e1-src lbl1 e1 DYN-TYPE REF-DYN-TYPE)
                        (mk-cast e2-src lbl2 e2 e2-ty DYN-TYPE))])]
         [else
          (error 'insert-casts/gbox-set!
                 "unexpected value for e1-ty: ~a"
                 e1-ty)])]
      [(Gvector (and (Ann _ (cons size-src size-ty))
                     (app ic-expr size))
                (app ic-expr e))
       (cond
         [(Dyn? size-ty)
          (define lbl (mk-label "gvector index" size-src))
          (Gvector (mk-cast size-src lbl size size-ty INT-TYPE) e)]
         [else (Gvector size e)])]
      [(Gvector-ref (and (Ann _ (cons e-src e-ty)) (app ic-expr e))
                    (and (Ann _ (cons i-src i-ty)) (app ic-expr i)))
       (define i-exp
         (cond
           [(Dyn? i-ty)
            (mk-cast i-src (mk-label "gvector-ref index" i-src) i i-ty INT-TYPE)]
           [else i]))
       (cond
         [(not (Dyn? e-ty)) (Gvector-ref e i-exp)]
         [else
          (define lbl (mk-label "gvector-ref" e-src))
          (cond
            [(dynamic-operations?)
             ;; TODO
             (Dyn-GVector-Ref e i-exp (lbl))]
            [else
             (Gvector-ref (mk-cast e-src lbl e e-ty (GVect DYN-TYPE)) i-exp)])])]
      [(Gvector-set! (and (Ann _ (cons e1-src e1-ty)) (app ic-expr e1))
                     (and (Ann _ (cons i-src i-ty)) (app ic-expr i))
                     (and (Ann _ (cons e2-src e2-ty)) (app ic-expr e2)))
       (define lbl1 (mk-label "gvector-set!" e1-src))
       (define lbl2 (mk-label "gvector-set!" e2-src))
       (define i-exp
         (cond
           [(Dyn? i-ty)
            (define lbl (mk-label "gvector-ref index" i-src))
            (mk-cast i-src lbl i i-ty INT-TYPE)]
           [else i]))
       (cond
         [(GVect? e1-ty)
          (Gvector-set! e1 i-exp (mk-cast e2-src lbl2 e2 e2-ty (GVect-arg e1-ty)))]
         [(Dyn? e1-ty)
          (cond
            [(dynamic-operations?)
             ;; TODO
             (Dyn-GVector-Set! e1 i-exp e2 e2-ty (lbl1))]
            [else
             (Gvector-set! (mk-cast e1-src lbl1 e1 DYN-TYPE (GVect DYN-TYPE))
                           i-exp
                           (mk-cast e2-src lbl2 e2 e2-ty DYN-TYPE))])]
         [else (error 'insert-casts/gvector-set!
                      "unexpected value for e1 type: ~a"
                      e1-ty)])]
      [(Mbox (app ic-expr e) t) (Mbox e t)]
      [(Munbox (app ic-expr e)) (Munbox e)]
      [(Mbox-set! (app ic-expr e1) (app ic-expr e2)) (Mbox-set! e1 e2)]
      [(MunboxT (and (Ann _ (cons e-src e-ty)) (app ic-expr e)) t)
       (cond
         [(Dyn? e-ty)
          (let ([addr (next-uid! "addr")])
            (Let `((,addr
                    .
                    ,(mk-cast e-src (mk-label "munbox" e-src) e e-ty (MRef DYN-TYPE))))
              (MBoxCastedRef addr t)))]
         [(MRef? e-ty)
          (match e
            [(Var addr) (MBoxCastedRef addr t)]
            [else (let ([addr (next-uid! "addr")])
                    (Let `((,addr . ,e))
                      (MBoxCastedRef addr t)))])]
         [else (error 'insert-casts/MunboxT
                      "unexpected value for e-ty: ~a"
                      e-ty)])]
      [(Mbox-set!T (and (Ann _ (cons e1-src e1-ty)) (app ic-expr e1))
                   (and (Ann _ (cons e2-src e2-ty)) (app ic-expr e2))
                   t)
       (let ([e2 (mk-cast e2-src (mk-label "val" e2-src) e2 e2-ty t)])
         (cond
           [(Dyn? e1-ty)
            (let ([addr (next-uid! "addr")])
              (Let `((,addr
                      .
                      ,(mk-cast e1-src (mk-label "mbox-set" e1-src) e1 e1-ty (MRef DYN-TYPE))))
                (MBoxCastedSet! addr e2 t)))]
           [(MRef? e1-ty) (match e1
                            [(Var addr) (MBoxCastedSet! addr e2 t)]
                            [else (let ([addr (next-uid! "mboxaddr")])
                                    (Let `((,addr . ,e1))
                                      (MBoxCastedSet! addr e2 t)))])]
           [else (error 'insert-casts/Mbox-set!T
                        "unexpected value for e1-ty: ~a"
                        e1-ty)]))]
      [(Mvector (and (Ann _ (cons size-src size-ty))
                     (app ic-expr size))
                (app ic-expr e)
                t)
       (cond
         [(Dyn? size-ty)
          (define lbl (mk-label "mvector index" size-src))
          (Mvector (mk-cast size-src lbl size size-ty INT-TYPE) e t)]
         [else (Mvector size e t)])]
      [(Mvector-ref (app ic-expr e1) (app ic-expr e2))
       (Mvector-ref e1 e2)]
      [(Mvector-set! (app ic-expr e1) (app ic-expr e2) (app ic-expr e3))
       (Mvector-set! e1 e2 e3)]
      [(Mvector-refT (and (Ann _ (cons e-src e-ty)) (app ic-expr e))
                     (and (Ann _ (cons i-src i-ty)) (app ic-expr i))
                     t)
       (let ([i (if (Dyn? i-ty)
                    (mk-cast i-src (mk-label "mvector-ref index" i-src) i i-ty INT-TYPE)
                    i)])
         (cond
           [(Dyn? e-ty)
            (let ([addr (next-uid! "addr")])
              (Let `((,addr
                      .
                      ,(mk-cast e-src (mk-label "mvector-ref" e-src) e e-ty (MVect DYN-TYPE))))
                (MVectCastedRef addr i t)))]
           [(MVect? e-ty) (let ([addr (next-uid! "addr")])
                            (Let `((,addr . ,e))
                              (MVectCastedRef addr i t)))]
           [else (error 'insert-casts/Mvector-refT
                        "unexpected value for e type: ~a"
                        e-ty)]))]
      [(Mvector-set!T (and (Ann _ (cons e1-src e1-ty)) (app ic-expr e1))
                      (and (Ann _ (cons i-src i-ty)) (app ic-expr i))
                      (and (Ann _ (cons e2-src e2-ty)) (app ic-expr e2))
                      t)
       (let ([i (if (Dyn? i-ty)
                    (mk-cast i-src (mk-label "mvector-set index" i-src) i i-ty INT-TYPE)
                    i)])
         (define lbl1 (mk-label "mvector-set!" e1-src))
         (define lbl2 (mk-label "mvector-set!" e2-src))
         (cond
           [(Dyn? e1-ty)
            (let ([addr (next-uid! "addr")])
              (Let `((,addr
                      .
                      ,(mk-cast e1-src (mk-label "mvector-set" e1-src) e1 e1-ty (MVect DYN-TYPE))))
                (MVectCastedSet! addr i (mk-cast e2-src lbl2 e2 e2-ty DYN-TYPE) t)))]
           [(MVect? e1-ty) (let ([e2c (mk-cast e2-src lbl2 e2 e2-ty (MVect-arg e1-ty))])
                             (let ([addr (next-uid! "mvectoraddr")])
                               (Let `((,addr . ,e1))
                                 (MVectCastedSet! addr i e2c t))))]
           [else (error 'insert-casts/Mvector-set!T
                        "unexpected value for e1 type: ~a"
                        e1-ty)]))]
      [(Mvector-length (and (Ann _ (cons e-src e-ty)) (app ic-expr e)))
       (if (Dyn? e-ty)
           (Mvector-length (mk-cast e-src (mk-label "mvector-length" e-src) e e-ty (MVect DYN-TYPE)))
           (Mvector-length e))]
      [(Gvector-length (and (Ann _ (cons e-src e-ty)) (app ic-expr e)))
       (if (Dyn? e-ty)
           (Gvector-length (mk-cast e-src (mk-label "gvector-length" e-src) e e-ty (GVect DYN-TYPE)))
           (Gvector-length e))]
      [(Create-tuple e*) (Create-tuple (map ic-expr e*))]
      [(Tuple-proj (and (Ann _ (cons e-src e-ty)) (app ic-expr e)) i)
       (cond
         [(Dyn? e-ty)
          (let ([n (+ i 1)])
            #;(unless (index? n) (error 'ic-expr "bad index"))
            (Tuple-proj (mk-cast e-src (mk-label "tuple-proj" e-src)
                                 e DYN-TYPE
                                 (STuple n (make-list n DYN-TYPE))) i))]
         [(STuple? e-ty) (Tuple-proj e i)]
         [else (error 'schml/insert-casts/ic-expr/Tuple-proj "unmatched: ~a" e-ty)])]
      ;; TODO add these cases when monotonic is finished
      ;;[(Mvector e1 e2)         (TODO define vector insert implicit casts)]
      ;;[(Mvector-ref e1 e2)     (TODO define vector insert implicit casts)]
      ;;[(Mvector-set! e1 e2 e3) (TODO define vector insert implicit casts)]
      [other (error 'insert-casts/expression "unmatched ~a" other)])))

(: make-list (Integer Schml-Type -> (Listof Schml-Type)))
(define (make-list n t)
  (if (= n 0)
      '()
      (cons t (make-list (- n 1) t))))

(: ic-bnd (S1-Bnd . -> . C0-Bnd))
(define (ic-bnd b)
  (match-let ([(Bnd i t (and rhs (Ann _ (cons rhs-src rhs-type)))) b])
    (cons i (mk-cast rhs-src (mk-label "binding" rhs-src) (ic-expr rhs) rhs-type t))))


;; An Application of dynamic casts the arguments to dyn and the operand to
;; a function that takes dynamic values and returns a dynamic value
(: ic-application (S1-Expr (Listof S1-Expr) Src Schml-Type . -> . C0-Expr))
(define (ic-application rator rand* src type)
  (match-let ([(Ann _ (cons rator-src rator-type)) rator])
    (cond
      [(Dyn? rator-type)
       (cond
         [(dynamic-operations?)
          (define-values (expr* type*) (ic-operands rand*))
          (define expr (ic-expr rator))
          (Dyn-Fn-App expr expr* type* ((mk-label "Application" src)))]
         [else
          (let*-values ([(exp* ty*) (ic-operands rand*)]
                        [(needed-rator-type) (Fn (length ty*) ty* DYN-TYPE)]
                        [(exp)          (ic-expr rator)]
                        [(lbl) (mk-label "Application" src rator-src)])
            (App (mk-cast rator-src lbl exp DYN-TYPE needed-rator-type) exp*))])]
      [(Fn? rator-type)
       ;(Ann type)
       (App (ic-expr rator)
            (map (ic-operand/cast src) rand* (Fn-fmls rator-type)))]
      [else (error 'unmatched "~a" rator-type)#;
            (raise-pass-exn "insert-casts"
                            "error in ic-application's implimentation")])))

(: make-dyn-fn-type (-> Index (Fn Index (Listof Schml-Type) Schml-Type)))
(define (make-dyn-fn-type n)
  (Fn n (build-list n (lambda (_) #;#;: Schml-Type DYN-TYPE)) DYN-TYPE))

(: ic-operands
   (-> (Listof S1-Expr)
       (values (Listof C0-Expr) (Listof Schml-Type))))
(define (ic-operands rand*)
  (for/lists (cf* ty*)
             #;
             ([cf* : (Listof C0-Expr)]
              [ty* : Schml-Type*])
    ([rand #;#;: S1-Expr rand*])
    (match-let ([(Ann _ (cons _ type)) rand])
      (values (ic-expr rand) type))))

(: ic-operand/cast (-> Src (-> S1-Expr Schml-Type C0-Expr)))
(define (ic-operand/cast app-src)
  (lambda (rand arg-type)
    (match-let ([(Ann _ (cons rand-src rand-type)) rand])
      (mk-cast rand-src (mk-label "application" app-src rand-src)
	       (ic-expr rand)
	       rand-type arg-type))))

(define-syntax-rule (th o ...)
  (lambda () o ...))

(define-syntax mk-label
  (syntax-rules ()
    [(_ pos src)
     (lambda ()
       (format "Implicit cast in ~a on expression at ~a\n"
               pos (srcloc->string src)))]
    [(_ pos sup-src sub-src)
     (mk-label (format "~a at ~a\n" pos (srcloc->string sup-src))
               sub-src)]))
