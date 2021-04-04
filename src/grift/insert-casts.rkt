;; There is a reported bug with contract generation
;; The current workaround is to use the nocheck language and avoid types.
#lang typed/racket/base/no-check
#|------------------------------------------------------------------------------+
|Pass: src/insert-casts                                                         
+-------------------------------------------------------------------------------+
|Author: Andre Kuhlenshmidt (akuhlens@indiana.edu)                              
+-------------------------------------------------------------------------------+
|Description: This pass implicit cast explicit via a inserted casts where types
| the value provided is not a subtype of the expected type, but is a 
| consistent-subtype of the expected type.
+------------------------------------------------------------------------------|#
(require
 (for-syntax racket/base)
 racket/match
 racket/port
 "./type-check.rkt"
 "../configuration.rkt"
 "../errors.rkt"
 "../language/forms.rkt"
 "../logging.rkt")

;; Only the pass is provided by this module
(provide
 insert-casts)

(: insert-casts (Grift1-Lang -> Cast0-Lang))
(define (insert-casts prgm)
  (define who 'insert-casts)
  (match-define (Prog (list name unique type) tl*) prgm)
  (define uc (make-unique-counter unique))
  (no-casts!)
  (define new-tl* : C0-Top*
    (parameterize ([current-unique-counter uc])
      (for/list ([tl (in-list tl*)])
        (match tl
          [(Define r? id t2 (and (Ann _ (cons s t1)) (app ic-expr e)))
           (Define r? id t2 (mk-cast s (mk-label "Define" s) e t1 t2))]
          [(Observe e t) (Observe (ic-expr e) t)]))))
  (when (and (or (program-must-be-statically-typed?)
                 (eq? (cast-representation) 'Static)) 
             (not (null? (unbox casts-inserted))))
    (define err-str
      (with-output-to-string
        (lambda ()
          (printf "~a is not statically typed\n" name)
          (display "\tneeded casts at the following locations:\n")
          (for ([loc.exp (in-list (reverse (unbox casts-inserted)))])
            (match-define (cons loc exp) loc.exp)
            (printf "\t\t~a\n" (srcloc->string loc))))))
    (error 'Type-Error err-str))

  (debug who (Prog (list name (unique-counter-next! uc) type) new-tl*)))


;; A list of casts that are inserted and their source location
(: casts-inserted (Boxof (Listof (cons srcloc C0-Expr))))
(define casts-inserted (box '()))
(: add-cast! : srcloc C0-Expr -> Void)
(define (add-cast! s e)
  (set-box! casts-inserted (cons (cons s e) (unbox casts-inserted))))
(: no-casts! : -> Void)
(define (no-casts!)
  (set-box! casts-inserted '()))


;; is `t1` a subtype of `t2`?
(define (subtype-of? t1 t2)
  (define (type=? a t1 t2)
    ;; I think this is a more optimized version of
    ;; (and (rec/a a t1 t2) (rec/a a t2 t1) a)
    (rec/a (rec/a a t1 t2) t2 t1))
  ;; substitute `t` for variables bound to this Mu to get
  ;; rid of this Mu scope.
  (define (rec/a-fold a t1 t2)
    (cond
      [(not a) #f]
      [else
       (match* (t1 t2)
         [('() _) a]
         [(_ '()) a]
         [((cons a1 d1) (cons a2 d2))
          (rec/a-fold (rec/a a1 a2 a) d1 d2)]
         [(_ _) (error 'rec/a-fold)])]))
  (define (rec/a t1 t2 a)
    (define (rec t1 t2)
      (cond
        [(equal? t1 t2) a]
        [(not (or (Mu? t1) (Mu? t2)))
         (match* (t1 t2)
           [((STuple n a1*) (STuple m a2*))
            #:when (<= m n)
            (rec/a-fold a a1* a2*)]
           ;; Contravariently function arguments
           [((Fn n a1* r1) (Fn n a2* r2))
            (rec/a-fold (rec r1 r2) a2* a1*)]
           ;; Invarient Reference types
           [((or (GVect a1) (GRef a1) (MVect a1) (MRef a1))
             (or (GVect a2) (GRef a2) (MVect a2) (MRef a2)))
            #:when (or (and (GVect? t1) (GVect? t2))
                       (and (GRef?  t1) (GRef?  t2))
                       (and (MVect? t1) (MVect? t2))
                       (and (MRef?  t1) (MRef?  t2)))
            (type=? a a1 a2)]
           [(_ _) #f])]
        [else
         (define p (cons t1 t2))
         (cond
           [(set-member? a p) a]
           [(Mu? t1) (rec/a (unfold-mu t1) t2 (set-add a p))]
           [(Mu? t2) (rec/a t1 (unfold-mu t2) (set-add a p))]
           [else (error 'subtype-of?)])]))
    (rec t1 t2))
  (debug 'subtype-of? t1 t2 (rec/a t1 t2 (set))))

(module+ test
  (require rackunit)
  (define inf-mt-stream (Mu (Scope (Fn 0 '() (TVar 0)))))
  (check-false
   (not (subtype-of? inf-mt-stream (Fn 0 '() inf-mt-stream))))
  (check-false
   (subtype-of? (Dyn) (Fn 0 '() (Fn 0 '() inf-mt-stream))))
  (check-false (subtype-of? (Dyn) inf-mt-stream))

  (check-false (not
                (subtype-of? (STuple 2 (list (Int) (Bool)))
                             (STuple 1 (list (Int))))))
  (check-false (subtype-of? (STuple 1 (list (Int)))
                            (STuple 2 (list (Int) (Bool)))))
  (check-false (not
                (subtype-of? (STuple 1 (list (Int)))
                             (STuple 1 (list (Int))))))
  (check-false (subtype-of? (STuple 1 (list (Dyn)))
                            (STuple 1 (list (Int)))))

  (check-false (subtype-of? (MRef (STuple 2 (list (Int) (Bool))))
                            (MRef (STuple 1 (list (Int))))))
  (check-false (subtype-of? (MRef (STuple 1 (list (Int))))
                            (MRef (STuple 2 (list (Int) (Bool))))))
  )

  
;; Make a cast from `t1` to `t2` of the value returned by `e`.
(: mk-cast (srcloc (-> Blame-Label) C0-Expr Grift-Type Grift-Type -> C0-Expr))
(define (mk-cast src l-th e t1 t2)
  (define who 'insert-casts/mk-cast)
  (cond
    [(subtype-of? t1 t2) (debug who t1 t2 e)]
    [else
     (define c (debug (Cast e (Twosome t1 t2 (l-th)))))
     (add-cast! src c)
     (debug who t1 t2 c)]))

(: ic-expr : G1-Ann-Expr -> C0-Expr)
(define (ic-expr exp^)
  (match-define (Ann exp (cons src (app unfold-possible-mu type))) exp^)
  (match exp
    [(Lambda fml* (and (Ann _ (cons body-src body-type))
                       (app ic-expr body)))
     (define fn-range (or (and (Fn? type) (Fn-ret type))
                          (error 'insert-casts "function type recieve non-function type")))
     (let* ([lbl-th (mk-label "lambda" body-src)]
            [body (mk-cast body-src lbl-th body
                           body-type (ann fn-range Grift-Type))]
            [fml* (map (inst Fml-identifier Uid Grift-Type) fml*)])
       (Lambda fml* body))]
    [(Let bnd* (and (Ann _ (cons src type^)) body))
     (Let (map ic-bnd bnd*)
       (mk-cast src (mk-label "let" src) (ic-expr body) type^ type))]
    [(Letrec bnd* (and (Ann _ (cons src type^)) body))
     (Letrec (map ic-bnd bnd*)
       (mk-cast src (mk-label "letrec" src) (ic-expr body) type^ type))]
    [(App rator rand*)
     (ic-application rator rand* src type)]
    [(Op (list prim rand-ty*) rand*)
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
     (Switch
      (mk-cast es (mk-label "switch" es) e et INT-TYPE)
      (for/list : (Switch-Case* C0-Expr) ([c c*])
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
       ;; Don't need to unfold-possible-mu here because canonical types
       ;; don't allow (Mu (Scope (Dyn))) to occur. Properly constructed
       ;; types would collapse that to just (Dyn). 
       [(Dyn? e-ty)
        (define lbl (mk-label "guarded unbox" e-src))
        (cond
          [(dynamic-operations?)
           (define dop (Dyn-GRef-Ref e (lbl)))
           (add-cast! e-src dop)
           dop]
          [else             
           (Gunbox (mk-cast e-src lbl e e-ty PBOX-DYN-TYPE))])]
       [else (Gunbox e)])]
    [(Gbox-set! (and (Ann _ (cons e1-src (app unfold-possible-mu e1-ty))) (app ic-expr e1))
                (and (Ann _ (cons e2-src e2-ty)) (app ic-expr e2)))
     (define lbl1 (mk-label "guarded box-set!" e1-src))
     (define lbl2 (mk-label "guarded box-set!" e2-src))
     (cond
       [(Dyn? e1-ty)
        (cond
          [(dynamic-operations?)
           (define dop (Dyn-GRef-Set! e1 e2 e2-ty (lbl1)))
           (add-cast! e1-src dop)
           dop]
          [else
           (Gbox-set! (mk-cast e1-src lbl1 e1 DYN-TYPE PBOX-DYN-TYPE)
                      (mk-cast e2-src lbl2 e2 e2-ty DYN-TYPE))])]
       [else
        (Gbox-set! e1 (mk-cast e2-src lbl2 e2 e2-ty (GRef-arg e1-ty)))])]
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
           (define dop (Dyn-GVector-Ref e i-exp (lbl)))
           (add-cast! e-src dop)
           dop]
          [else
           (Gvector-ref (mk-cast e-src lbl e e-ty (GVect DYN-TYPE)) i-exp)])])]
    [(Gvector-set! (and (Ann _ (cons e1-src (app unfold-possible-mu e1-ty)))
                        (app ic-expr e1))
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
       [(Dyn? e1-ty)
        (cond
          [(dynamic-operations?)
           (define dop (Dyn-GVector-Set! e1 i-exp e2 e2-ty (lbl1)))
           (add-cast! e1-src dop)
           dop]
          [else
           (Gvector-set! (mk-cast e1-src lbl1 e1 DYN-TYPE (GVect DYN-TYPE))
                         i-exp
                         (mk-cast e2-src lbl2 e2 e2-ty DYN-TYPE))])]
       [else
        (Gvector-set! e1 i-exp (mk-cast e2-src lbl2 e2 e2-ty (GVect-arg e1-ty)))])]
    [(Mbox (app ic-expr e) t) (Mbox e t)]
    [(Munbox (and (Ann _ (cons e-src (app unfold-possible-mu e-ty)))
                  (app ic-expr e)))
     ;; It would be nice if I can insert the cast from the runtime
     ;; type here but that will ultimately expose the runtime
     ;; operations so early and has little benefit because the
     ;; source type is not known at this point anyway. However, we
     ;; can detect injections in the coercions case to be optimized
     ;; later by some cast optimizer.
     (match e-ty
       [(Dyn)
        (let ([lbl (mk-label "munbox" e-src)])
          (cond
            [(dynamic-operations?)
             (define dop (Dyn-MRef-Ref e (lbl)))
             (add-cast! e-src dop)
             dop]
            [else
             (define cst (mk-cast e-src lbl e DYN-TYPE (MRef DYN-TYPE)))
             (MBoxCastedRef cst DYN-TYPE)]))]
       [(MRef t)
        (if (completely-static-type? t)
            (Munbox e)
            (MBoxCastedRef e t))]
       [else (error 'insert-casts/MunboxT
                    "unexpected value for e-ty: ~a"
                    e-ty)])]
    [(Mbox-set! (and (Ann _ (cons e1-src (app unfold-possible-mu e1-ty)))
                     (app ic-expr e1))
                (and (Ann _ (cons e2-src e2-ty)) (app ic-expr e2)))
     (define e2-lbl (mk-label "val" e2-src))
     (match e1-ty
       [(Dyn)
        (let ([lbl (mk-label "mbox-set" e1-src)])
          (cond
            [(dynamic-operations?)
             (define dop (Dyn-MRef-Set! e1 e2 e2-ty (lbl)))
             (add-cast! e1-src dop)
             dop]
            [else
             (MBoxCastedSet!
              (mk-cast e1-src lbl e1 DYN-TYPE (MRef DYN-TYPE))
              (mk-cast e2-src e2-lbl e2 e2-ty DYN-TYPE)
              DYN-TYPE)]))]
       [(MRef t)
        (if (completely-static-type? t)
            (Mbox-set! e1 (mk-cast e2-src e2-lbl e2 e2-ty t))
            ;; OPTIMIZATION: instead of casting e2 from
            ;; e2-ty to t, then cast it again from t to
            ;; the runtime type, I cast it from e2-ty to
            ;; the runtime type directly.
            ;; Justification: the runtime type is at
            ;; least as precise as t by semantics.
            (MBoxCastedSet! e1 e2 e2-ty))])]
    [(Mvector (and (Ann _ (cons size-src size-ty))
                   (app ic-expr size))
              (app ic-expr e)
              t)
     (cond
       [(Dyn? size-ty)
        (define lbl (mk-label "mvector index" size-src))
        (Mvector (mk-cast size-src lbl size size-ty INT-TYPE) e t)]
       [else (Mvector size e t)])]
    [(Mvector-ref (and (Ann _ (cons e-src (app unfold-possible-mu e-ty)))
                       (app ic-expr e))
                  (and (Ann _ (cons i-src i-ty)) (app ic-expr i)))
     (define i^
       (cond
         [(Dyn? i-ty)
          (define lbl (mk-label "mvector-ref index" i-src))
          (mk-cast i-src lbl i i-ty INT-TYPE)]
         [else i]))
     (match e-ty
       [(Dyn)
        (define lbl (mk-label "mvect-ref" e-src)) 
        (cond
          [(dynamic-operations?)
           (define dop (Dyn-MVector-Ref e i^ (lbl)))
           (add-cast! e-src dop)
           dop]
          [else
           (define cst (mk-cast e-src lbl e DYN-TYPE (MVect DYN-TYPE)))
           (MVectCastedRef cst i^ DYN-TYPE)])]
       [(MVect t)
        (cond
          [(completely-static-type? t) (Mvector-ref e i^)]
          [else (MVectCastedRef e i^ t)])])]
    [(Mvector-set! (and (Ann _ (cons e1-src (app unfold-possible-mu e1-ty)))
                        (app ic-expr e1))
                   (and (Ann _ (cons i-src i-ty)) (app ic-expr i))
                   (and (Ann _ (cons e2-src e2-ty)) (app ic-expr e2)))
     (define i^
       (cond
         [(Dyn? i-ty)
          (define lbl (mk-label "mvector-set index" i-src))
          (mk-cast i-src lbl i i-ty INT-TYPE)]
         [else i]))
     (define e2-lbl (mk-label "val" e2-src))
     (match e1-ty
       [(Dyn)
        (define lbl (mk-label "mvect-set" e1-src))
        (cond
          [(dynamic-operations?)
           (define dop (Dyn-MVector-Set! e1 i^ e2 e2-ty (lbl)))
           (add-cast! e1-src dop)
           dop]
          [else
           (define e1^ (mk-cast e1-src lbl e1 DYN-TYPE (MVect DYN-TYPE)))
           (define e2^ (mk-cast e2-src e2-lbl e2 e2-ty DYN-TYPE)) 
           (MVectCastedSet! e1^ i e2^ DYN-TYPE)])]
       [(MVect t)
        (if (completely-static-type? t)
            (Mvector-set! e1 i^ (mk-cast e2-src e2-lbl e2 e2-ty t))
            (MVectCastedSet! e1 i^ e2 e2-ty))])]
    [(Mvector-length (and (Ann _ (cons e-src (Dyn))) (app ic-expr e)))
     (define l-th (mk-label "mvector-length" e-src))
     (Mvector-length (mk-cast e-src l-th e DYN-TYPE MVEC-DYN-TYPE))]
    [(Mvector-length (app ic-expr e))
     (Mvector-length e)]
    [(Gvector-length (and (Ann _ (cons e-src (Dyn))) (app ic-expr e)))
     (define l-th (mk-label "gvector-length" e-src))
     (cond
       [(dynamic-operations?) 
        (define dop (Dyn-GVector-Len e (Quote (l-th))))
        (add-cast! e-src dop)
        dop]
       [else (Gvector-length (mk-cast e-src l-th e DYN-TYPE PVEC-DYN-TYPE))])]
    [(Gvector-length (app ic-expr e))
     (Gvector-length e)]
    [(Create-tuple e*) (Create-tuple (map ic-expr e*))]
    [(Tuple-proj (and (Ann _ (cons e-src (Dyn))) (app ic-expr e)) i)
     (define l-th (mk-label "tuple-proj" e-src))
     (cond
       [(dynamic-operations?)
        (define dop : C0-Expr
          (Dyn-Tuple-Proj e (Quote i) (Quote (l-th))))
        (add-cast! e-src dop)
        dop]
       [else
        (define n : Index (add1 (cast i Index)))
        (define tgt-ty : Grift-Type
          (STuple n (make-list n DYN-TYPE)))
        (Tuple-proj (mk-cast e-src l-th e DYN-TYPE tgt-ty) i)])]
    [(Tuple-proj (app ic-expr e) i)
     (Tuple-proj e i)]
    [other (error 'insert-casts/expression "unmatched ~a" other)]))

(: make-list (Integer Grift-Type -> (Listof Grift-Type)))
(define (make-list n t)
  (if (= n 0)
      '()
      (cons t (make-list (- n 1) t))))

(: ic-bnd (G1-Bnd -> C0-Bnd))
(define (ic-bnd b)
  (match-define (Bnd i t (and rhs (Ann _ (cons rhs-src rhs-type)))) b) 
  (define lbl (mk-label "binding" rhs-src))
  (define rhs^ (ic-expr rhs))
  (cons i (mk-cast rhs-src lbl rhs^ rhs-type t)))


;; An Application of dynamic casts the arguments to dyn and the operand to
;; a function that takes dynamic values and returns a dynamic value
(: ic-application (G1-Ann-Expr (Listof G1-Ann-Expr) Src Grift-Type . -> . C0-Expr))
(define (ic-application rator rand* src type)
  (match-define (Ann _ (cons rator-src (app unfold-possible-mu rator-type)))
    rator)
  (match rator-type
    [(Dyn)
     (cond
       [(dynamic-operations?)
        (define-values (exp* ty*) (ic-operands rand*))
        (define exp (ic-expr rator))
        (define lbl ((mk-label "Application" src)))
        (define dop (Dyn-Fn-App exp exp* ty* lbl))
        (add-cast! rator-src dop)
        dop]
       [else
        (define-values (exp* ty*) (ic-operands rand*))
        (define needed-rator-type (Fn (length ty*) ty* DYN-TYPE))
        (define exp (ic-expr rator))
        (define lbl (mk-label "Application" src rator-src))
        (define exp^ (mk-cast rator-src lbl exp DYN-TYPE needed-rator-type))
        (App exp^ exp*)])]
    [(Fn _ dom _)
     (define exp* (map (ic-operand/cast src) rand* dom))
     (App (ic-expr rator) exp*)]))
    

(: make-dyn-fn-type (-> Index (Fn Index (Listof Grift-Type) Grift-Type)))
(define (make-dyn-fn-type n)
  (Fn n (build-list n (lambda (_) #;#;: Grift-Type DYN-TYPE)) DYN-TYPE))

(: ic-operands
   (-> (Listof G1-Ann-Expr)
       (values (Listof C0-Expr) (Listof Grift-Type))))
(define (ic-operands rand*)
  (for/lists ([cf* : (Listof C0-Expr)] [ty* : (Listof Grift-Type)])
             ([rand rand*])
    (match-let ([(Ann _ (cons _ type)) rand])
      (values (ic-expr rand) type))))

(: ic-operand/cast (-> Src (-> G1-Ann-Expr Grift-Type C0-Expr)))
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



