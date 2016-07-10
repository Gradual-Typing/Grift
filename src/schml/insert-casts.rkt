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

;; Only the pass is provided by this module
(provide insert-casts
         (all-from-out
          "../language/schml1.rkt"
          "../language/cast0.rkt"))



(: insert-casts (Schml1-Lang Config . -> . Cast0-Lang))
(define (insert-casts prgm comp-config)
  (match-let ([(Prog (list name next-uid type) exp) prgm])
    (Prog (list name next-uid type) (iic-expr exp))))

(: mk-cast ((-> Blame-Label) C0-Expr Schml-Type Schml-Type . -> . C0-Expr))
(define (mk-cast l-th e t1 t2)
  (if (equal? t1 t2) e (Cast e (Twosome t1 t2 (l-th)))))

(: iic-expr (S1-Expr . -> . C0-Expr))
(define (iic-expr exp^)
  (match-let ([(Ann exp (cons src type)) exp^])
    (match exp
      [(Lambda fml* (and (Ann _ (cons body-src body-type))
                         (app iic-expr body)))
       (unless (Fn? type)
         (error 'insert-casts "function type recieve non-function type"))
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
          (define lbl (mk-label "guarded unbox" e-src))
          (cond
            [(dynamic-operations?)
             (Dyn-GRef-Ref e (lbl))]
            [else             
             (Gunbox (mk-cast lbl e e-ty (GRef DYN-TYPE)))])]
         [else
          (error 'insert-casts/gunbox "unexexpected value for e-ty: ~a" e-ty)])]
      [(Gbox-set! (and (Ann _ (cons e1-src e1-ty)) (app iic-expr e1))
                  (and (Ann _ (cons e2-src e2-ty)) (app iic-expr e2)))
       (define lbl1 (mk-label "guarded box-set!" e1-src))
       (define lbl2 (mk-label "guarded box-set!" e2-src))
       (cond
         [(GRef? e1-ty)
          (Gbox-set! e1 (mk-cast lbl2 e2 e2-ty (GRef-arg e1-ty)))]
         [(Dyn? e1-ty)
          (cond
            [(dynamic-operations?)
             (Dyn-GRef-Set! e1 e2 e2-ty (lbl1))]
            [else             
             (Gbox-set! (mk-cast lbl1 e1 DYN-TYPE REF-DYN-TYPE)
                        (mk-cast lbl2 e2 e2-ty DYN-TYPE))])]
         [else
          (error 'insert-casts/gbox-set!
                 "unexpected value for e1-ty: ~a"
                 e1-ty)])]
      [(Gvector (and (Ann _ (cons size-src size-ty))
                     (app iic-expr size))
                (app iic-expr e))
       (cond
         [(Dyn? size-ty)
          (define lbl (mk-label "gvector index" size-src))
          (Gvector (mk-cast lbl size size-ty INT-TYPE) e)]
         [else (Gvector size e)])]
      [(Gvector-ref (and (Ann _ (cons e-src e-ty)) (app iic-expr e))
                    (and (Ann _ (cons i-src i-ty)) (app iic-expr i)))
       (define i-exp
         (cond
           [(Dyn? i-ty)
            (mk-cast (mk-label "gvector-ref index" i-src) i i-ty INT-TYPE)]
           [else i]))
       (cond
         [(not (Dyn? e-ty)) (Gvector-ref e i-exp)]
         [else
          (define lbl (mk-label "gvector-ref" e-src))
          (cond
            [(dynamic-operations?)
             (Dyn-GVector-Ref e i-exp (lbl))]
            [else (Gvector-ref (mk-cast lbl e e-ty (GVect DYN-TYPE)) i-exp)])])]
      [(Gvector-set! (and (Ann _ (cons e1-src e1-ty)) (app iic-expr e1))
                     (and (Ann _ (cons i-src i-ty)) (app iic-expr i))
                     (and (Ann _ (cons e2-src e2-ty)) (app iic-expr e2)))
       (define lbl1 (mk-label "gvector-set!" e1-src))
       (define lbl2 (mk-label "gvector-set!" e2-src))
       (define i-exp
         (cond
           [(Dyn? i-ty)
            (define lbl (mk-label "gvector-ref index" i-src))
            (mk-cast lbl i i-ty INT-TYPE)]
           [else i]))
       (cond
         [(GVect? e1-ty)
          (Gvector-set! e1 i-exp (mk-cast lbl2 e2 e2-ty (GVect-arg e1-ty)))]
         [(Dyn? e1-ty)
          (cond
            [(dynamic-operations?)
             (Dyn-GVector-Set! e1 i-exp e2 e2-ty (lbl1))]
            [else
             (Gvector-set! (mk-cast lbl1 e1 DYN-TYPE (GVect DYN-TYPE))
                           i-exp
                           (mk-cast lbl2 e2 e2-ty DYN-TYPE))])]
         [else (error 'insert-casts/gvector-set!
                      "unexpected value for e1 type: ~a"
                      e1-ty)])]
      [(Create-tuple e*) (Create-tuple (map iic-expr e*))]
      [(Tuple-proj (and (Ann _ (cons e-src e-ty)) (app iic-expr e)) i)
       (cond
         [(Dyn? e-ty)
          (let ([n (+ i 1)])
            (unless (index? n) (error 'iic-expr "bad index"))
            (Tuple-proj (mk-cast (mk-label "tuple-proj" e-src)
                                 e DYN-TYPE
                                 (STuple n (make-list n DYN-TYPE))) i))]
         [(STuple? e-ty) (Tuple-proj e i)]
         [else (TODO error message that is appropriate)])]
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
       (cond
         [(dynamic-operations?)
          (define-values (expr* type*) (iic-operands rand*))
          (define expr (iic-expr rator))
          (Dyn-Fn-App expr expr* type* ((mk-label "Application" src)))]
         [else
          (let*-values ([(exp* ty*) (iic-operands rand*)]
                        [(needed-rator-type) (Fn (length ty*) ty* DYN-TYPE)]
                        [(exp)          (iic-expr rator)]
                        [(lbl) (mk-label "Application" src rator-src)])
            (App (mk-cast lbl exp DYN-TYPE needed-rator-type) exp*))])]
      [(Fn? rator-type)
       ;(Ann type)
       (App (iic-expr rator)
            (map (iic-operand/cast src) rand* (Fn-fmls rator-type)))]
      [else (raise-pass-exn "insert-casts"
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
