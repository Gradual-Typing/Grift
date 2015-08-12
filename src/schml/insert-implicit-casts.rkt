#lang typed/racket
#|------------------------------------------------------------------------------+
|Pass: src/insert-implicit-casts                                           |
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              |
+-------------------------------------------------------------------------------+
|Discription: This pass converts differences in type equality via a inserted
|cast. After this pass all types should be explicit and the program should be
|roughly equivalent to the cast-calculus mentioned in papers on the gradually
|typed lambda calculus.
+-------------------------------------------------------------------------------+
|Input Grammar
+------------------------------------------------------------------------------|#
(require "../helpers.rkt"
         "../errors.rkt"
         "../language.rkt")

;; Only the pass is provided by this module
(provide insert-implicit-casts)

(: insert-implicit-casts (Schml1-Lang Config . -> . Cast0-Lang))
(define (insert-implicit-casts prgm comp-config)
  (match-let ([(Prog (list name next-uid type) exp) prgm])
    (Prog (list name next-uid type) (iic-expr exp))))

(: mk-cast ((-> String) C0-Expr Schml-Type Schml-Type . -> . C0-Expr))
(define (mk-cast l-th e t1 t2)
  (if (equal? t1 t2) e (Cast e t1 t2 (l-th))))

(: iic-expr (S1-Expr . -> . C0-Expr))
(define (iic-expr exp^)
  (match-let ([(Ann exp (cons src type)) exp^])
    (match exp
      [(Lambda fml* (and (Ann _ (cons body-src body-type))
                         (app iic-expr body)))
       (unless (Fn? type) (TODO error really big here))
       (let* ([lbl-th (mk-label "lambda" body-src)]
              [body (mk-cast lbl-th body body-type (ann (Fn-ret type) Schml-Type))]
              [fml* : Uid* (map (inst Fml-identifier Uid Schml-Type) fml*)])
         (Lambda fml* body))]
      [(Let bnd* (and (Ann _ (cons src type^)) body))
       (Let (map iic-bnd bnd*) (mk-cast (mk-label "let" src) (iic-expr body) type^ type))]
      [(Letrec bnd* (and (Ann _ (cons src type^)) body))
       (Letrec (map iic-bnd bnd*)
               (mk-cast (mk-label "letrec" src) (iic-expr body) type^ type))]
      [(App rator rand*) (iic-application rator rand* src type)]
      [(Op (Ann prim rand-ty*) rand*)
       (Op prim (map (iic-operand/cast src) rand* rand-ty*))]
      [(Ascribe exp t1 lbl?)
       (let ([lbl (if lbl? (th lbl?) (mk-label "ascription" src))])
         (mk-cast lbl (iic-expr exp) t1 type))]
      [(If (and (Ann _ (cons tst-src tst-ty)) (app iic-expr tst))
           (and (Ann _ (cons csq-src csq-ty)) (app iic-expr csq))
           (and (Ann _ (cons alt-src alt-ty)) (app iic-expr alt)))
       (If (mk-cast (mk-label "if" tst-src) tst tst-ty BOOL-TYPE)
           (mk-cast (mk-label "if" csq-src) csq csq-ty type)
           (mk-cast (mk-label "if" alt-src) alt alt-ty type))]
      [(Var id) (Var id)]
      [(Quote lit) (Quote lit)]
      [(Begin e* e) (Begin (map iic-expr e*) (iic-expr e))]
      [(Repeat id (and (Ann _ (cons s1 t1)) (app iic-expr e1))
               (and (Ann _ (cons s2 t2)) (app iic-expr e2))
               (app iic-expr e3))
       (Repeat id
               (mk-cast (mk-label "Repeat" s1) e1 t1 INT-TYPE)
               (mk-cast (mk-label "Repeat" s2) e2 t2 INT-TYPE)
               e3)]
      ;; These rules are a rough translation of the coercion semantics that
      ;; are presented in space-efficient gradual typing figure 4
      ;; Their system was using lazy-ud and this system is lazy-d
      [(Gbox e) (Gbox (iic-expr e))]
      [(Gunbox (and (Ann _ (cons e-src e-ty)) (app iic-expr e)))
       (cond
         [(GRef? e-ty) (Gunbox e)]
         [(Dyn? e-ty)
          (Gunbox (mk-cast (mk-label "guarded unbox" e-src) e e-ty (GRef DYN-TYPE)))]
         [else (TODO come up with an error)])]
      [(Gbox-set! (and (Ann _ (cons e1-src e1-ty)) (app iic-expr e1))
                  (and (Ann _ (cons e2-src e2-ty)) (app iic-expr e2)))
       (let ([lbl1 (mk-label "guarded box-set!" e1-src)]
             [lbl2 (mk-label "guarded box-set!" e2-src)])
         (cond
           [(GRef? e1-ty)
            (Gbox-set! e1 (mk-cast lbl2 e2 e2-ty (GRef-arg e1-ty)))]
           [(Dyn? e1-ty)
            (Gbox-set! (mk-cast lbl1 e1 DYN-TYPE REF-DYN-TYPE)
                       (mk-cast lbl2 e2 e2-ty DYN-TYPE))]
           [else (TODO error message that is appropriate)]))]
      ;; These rules bastardizations of the rules found in Figure 6 of
      ;; Monotonic references for efficient gradual typing
      [(Mbox (and (Ann _ (cons e-src e-ty)) (app iic-expr e)))
       (Mbox (Ann e (cons ((mk-label "monotonic box" e-src)) e-ty)))]
      [(Munbox (and (Ann _ (cons e-src e-ty)) (app iic-expr e)))
       (if (completely-static-type? e-ty)
           ;; if the types are static then in order to typecheck they must have
           ;; been consistent with (Ref type)
           (Munbox e)
           (Munbox
            (Ann (mk-cast (mk-label "monotonic unbox" e-src) e e-ty (MRef type))
                 (cons ((mk-label "monotonic unbox" e-src)) type))))]
      [(Mbox-set! (and (Ann _ (cons e1-src e1-ty)) (app iic-expr e1))
                  (and (Ann _ (cons e2-src e2-ty)) (app iic-expr e2)))
       (TODO come up with a reasonable version of this)]
      [(Gvector size e) (Gvector (iic-expr size) (iic-expr e))]
      [(Gvector-ref (and (Ann _ (cons e-src e-ty)) (app iic-expr e)) (app iic-expr ind))
       (cond
         [(GVect? e-ty) (Gvector-ref e ind)]
         [(Dyn? e-ty)
          (Gvector-ref (mk-cast (mk-label "guarded vector ref" e-src) e e-ty (GVect DYN-TYPE)) ind)]
         [else (TODO come up with an error)])]
      [(Gvector-set! (and (Ann _ (cons e1-src e1-ty)) (app iic-expr e1))
                     (app iic-expr ind)
                     (and (Ann _ (cons e2-src e2-ty)) (app iic-expr e2)))
       (let ([lbl1 (mk-label "guarded vector-set!" e1-src)]
             [lbl2 (mk-label "guarded vector-set!" e2-src)])
         (cond
           [(GVect? e1-ty)
            (Gvector-set! e1 ind (mk-cast lbl2 e2 e2-ty (GVect-arg e1-ty)))]
           [(Dyn? e1-ty)
            (Gvector-set! (mk-cast lbl1 e1 DYN-TYPE (GVect DYN-TYPE))
                          ind
                          (mk-cast lbl2 e2 e2-ty DYN-TYPE))]
           [else (TODO error message that is appropriate)]))]
      [(Mvector e1 e2)         (TODO define vector insert implicit casts)]
      [(Mvector-ref e1 e2)     (TODO define vector insert implicit casts)]
      [(Mvector-set! e1 e2 e3) (TODO define vector insert implicit casts)])))

(: iic-bnd (S1-Bnd . -> . C0-Bnd))
(define (iic-bnd b)
  (match-let ([(Bnd i t (and rhs (Ann _ (cons rhs-src rhs-type)))) b])
    (cons i (mk-cast (mk-label "binding" rhs-src) (iic-expr rhs) rhs-type t))))


;; An Application of dynamic casts the arguments to dyn and the operand to
;; a function that takes dynamic values and returns a dynamic value
(: iic-application (S1-Expr (Listof S1-Expr) Src Schml-Type . -> . C0-Expr))
(define (iic-application rator rand* src type)
  (match-let ([(Ann _ (cons rator-src rator-type)) rator])
    (cond
      [(Dyn? rator-type)
       (let*-values ([(exp* ty*) (iic-operands rand*)]
                     [(needed-rator-type) (Fn (length ty*) ty* DYN-TYPE)]
                     [(exp)          (iic-expr rator)]
                     [(lbl) (mk-label "Application" src rator-src)])
         (App (mk-cast lbl exp DYN-TYPE needed-rator-type) exp*))]
      ;; This is what this code needs to do. I will talk to michael about this
      ;; and see that this is really needed.
      #|
      [(Dyn? rator-type)
      (let ([rator-type (make-dyn-fn-type (length rand*))])
      (App (mk-cast (mk-label "Application" src rator-src)
      (iic-expr rator)
      rator-type
      (make-dyn-fn-type (length rand*)))
      (map (iic-operand/cast src) rand* (Fn-fmls rator-type))))]
      |#
      [(Fn? rator-type)
       ;(Ann type)
       (App (iic-expr rator)
            (map (iic-operand/cast src) rand* (Fn-fmls rator-type)))]
      [else (raise-pass-exn "insert-implicit-casts"
                            "error in iic-application's implimentation")])))

(: make-dyn-fn-type (-> Index (Fn Index (Listof Schml-Type) Schml-Type)))
(define (make-dyn-fn-type n)
  (Fn n (build-list n (lambda (_) : Schml-Type DYN-TYPE)) DYN-TYPE))

(: iic-operands
   (-> (Listof S1-Expr)
       (values (Listof C0-Expr) (Listof Schml-Type))))
(define (iic-operands rand*)
  (for/lists ([cf* : (Listof C0-Expr)]
	      [ty* : Schml-Type*])
             ([rand : S1-Expr rand*])
    (match-let ([(Ann _ (cons _ type)) rand])
      (values (iic-expr rand) type))))

(: iic-operand/cast (-> Src (-> S1-Expr Schml-Type C0-Expr)))
(define (iic-operand/cast app-src)
  (lambda (rand arg-type)
    (match-let ([(Ann _ (cons rand-src rand-type)) rand])
      (mk-cast (mk-label "application" app-src rand-src)
	       (iic-expr rand)
	       rand-type arg-type))))

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
