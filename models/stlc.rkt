#lang racket

(require "helpers.rkt" (for-syntax racket/syntax))

(provide (all-from-out "helpers.rkt"))

(provide
 ;; The syntax of the simply typeded lambda calculus
 STLC STLC/tc
 ;; Alpha equivalence
 ;; (=α t1 t2) : metafunction
 ;; is t1 alpha equivalent to t2? 
 =α
 ;; (=α/racket t1 t2) racket function
 ;; is t1 alpha equivalent to t2? 
 =α/racket
 ;; (⊢ ((xs Ts) ...) e T) : Judgement (⊢ I I O)
 ;; from the environment, ((xs Ts) ...), we can infer that expression
 ;; e is of type T.
 ⊢
 ;; Reduction relation for the calculus
 -->β
 ;; Standard reduction relation for the call-by value semantics
 s->βv
 ;; (tyop o) what is the type of o.
 tyop)

(define-language STLC
  ;; Things that should definately not be included in the grammar
  ;; if produced as a pic or pdf
  (Γ ::= · (x any Γ))
  (x ::= variable-not-otherwise-mentioned)
  (i ::= integer)
  (b ::= boolean)
  (k ::= i   b    ())
  (B ::= Int Bool ())
  ;; End things that must not be named 
  (e   ::= k x f
       (letrec  ([x : T f]) e)
       (letrec2 ([x_!_ : T f] [x_!_ : T f]) e)
       (let ([x : T e]) e)
       (let2 ([x_!_ : T e] [x_!_ : T e]) e)
       (o e e)
       (e e)
       (if e e e))
  (f ::= (lambda ([x : T]) e))
  ;;
  (o ::= + - * =)
  ;; Types
  (T ::= B (T -> T))
  #:binding-forms
  (lambda ([x_param : T_param]) e #:refers-to x_param)
  (let ([x_bnd : T_bnd e_bnd])
    e #:refers-to x_bnd)
  (let2 ([x_1 : T_1 e_1]
         [x_2 : T_2 e_2])
        e #:refers-to (shadow x_1 x_2))
  (letrec ([x_bnd : T_bnd e_bnd #:refers-to x_bnd])
    e #:refers-to x_bnd)
  (letrec2 ([x_1 : T_1 e_1 #:refers-to (shadow x_1 x_2)]
            [x_2 : T_2 e_2 #:refers-to (shadow x_1 x_2)])
    e #:refers-to (shadow x_1 x_2)))

(module+ test
  (test-true
   (alpha-equivalent? STLC
    (term (lambda ([x : Int]) x))
    (term (lambda ([y : Int]) y))))
  (test-true
   (alpha-equivalent? STLC
    (term (lambda ([x : Int]) (lambda ([y : Int]) y)))
    (term (lambda ([y : Int]) (lambda ([y : Int]) y)))))
  (test-false
   (alpha-equivalent?
    STLC
    (term (lambda ([x : Int]) (lambda ([y : Int]) y)))
    (term (lambda ([x : Int]) (lambda ([y : Int]) x)))))
  (test-true
   (alpha-equivalent?
    STLC
    (term (let ([x : Int 1]) x))
    (term (let ([y : Int 1]) y))))
  (test-true
   (alpha-equivalent?
    STLC
    (term (letrec ([x : Int x]) x))
    (term (letrec ([y : Int y]) y))))
  (test-true
   (alpha-equivalent?
    STLC
    (term (letrec2 ([x : Int y] [y : Int x]) x))
    (term (letrec2 ([y : Int x] [x : Int y]) y))))
  (test-false
   (alpha-equivalent?
    STLC
    (term (letrec2 ([x : Int y] [y : Int x]) x))
    (term (letrec2 ([x : Int y] [y : Int x]) y)))))

;;———————————————————————————————————————————————————–———————————————————–——————
;; Scope helpers

#;
(define-metafunction STLC
  in : any (any ...) -> boolean
  [(in any_1 (any_n ... any_1 any_m ...)) #t]
  [(in _ _) #f])


(module+ test
  ;; Example programs to test fvs, bvs, and =α
  (define xfree0  (term (lambda ([y : ()]) (lambda ([z : ()]) x))))
  (define xfree1  (term ((lambda ([x : ()]) x) x)))
  (define x!free0 (term (lambda ([x : ()]) x)))
  (define x!free1 (term (lambda ([x : ()]) (x x))))

  (define xbound0  (term (lambda ([x : ()]) (x x))))
  (define xbound1  (term ((lambda ([x : ()]) x) x)))
  (define x!bound0 (term (lambda ([y : ()]) (lambda ([z : ()]) x))))
  (define x!bound1 (term ((lambda ([x : ()]) z) x)))

  (define fst0  (term (lambda ([x : ()]) (lambda ([y : ()]) x))))
  (define fst1  (term (lambda ([n : ()]) (lambda ([m : ()]) n))))
  (define snd0  (term (lambda ([x : ()]) (lambda ([y : ()]) y))))
  (define snd1  (term (lambda ([n : ()]) (lambda ([m : ()]) m))))

  (test-true (redex-match? STLC e fst0))
  (test-true (redex-match? STLC e fst1))
  (test-true (redex-match? STLC e snd0))
  (test-true (redex-match? STLC e snd1))

  )


;; fvs : Collect the unique free variables of a gradual expression
#;
(define-metafunction STLC
  fvs : any -> (x_!_ ...)
  [(fvs x) (x)]
  [(fvs (lambda ([x : _] ...) any))
   ,(set-subtract (term (fvs any)) (term (x ...)))]
  [(fvs (any ...))
   ,(foldr set-union (term ()) (term ((fvs any) ...)))])

;; fv? : Does a variable occur free in a expression?
#;
(define-metafunction STLC
  fv? : x any -> boolean
  [(fv? x any) (in x (fvs any))])

#;
(module+ test
  (test-true  (term (fv? x ,xfree0)))
  (test-true  (term (fv? x ,xfree1)))
  (test-false (term (fv? x ,x!free0)))
  (test-false (term (fv? x ,x!free1))))

;; bvs : return the bound variables of an expression
#;
(define-metafunction STLC
  bvs : any -> (x_!_ ...)
  [(bvs x) ()]
  [(bvs (lambda ([x : _] ...) any))
   ,(set-union (set-intersect (term (fvs any)) (term (x ...)))
               (term (bvs any)))]
  [(bvs (any ...)) ,(foldr set-union (term ()) (term ((bvs any) ...)))])

;; bv? : test if a variable occurs bound in an expression
#;
(define-metafunction STLC
  bv? : x any -> boolean
  [(bv? x any) (in x (bvs any))])

#;
(module+ test
  (test-true  (term (bv? x ,xbound0)))
  (test-true  (term (bv? x ,xbound1)))
  (test-false (term (bv? x ,x!bound0)))
  (test-false (term (bv? x ,x!bound1)))
  (get-coverage))


;; =α : are two expression alpha equivalent?
(define-metafunction STLC
  =α : any any -> boolean
  [(=α any any) #t]
  [(=α _   _)   #f])

(define (=α/racket t1 t2) (term (=α ,t1 ,t2)))

(module+ test
  (test-true (term (=α (lambda ([x : ()]) x)
                       (lambda ([y : ()]) y))))
  (test-false (term (=α (lambda ([x : ()]) y)
                        (lambda ([y : ()]) y))))
  (test-true (term (=α (lambda ([x : ()]) x)
                       (lambda ([x : ()]) x))))
  (test-true (term (=α (lambda ([x : ()]) (lambda ([y : ()]) (x y)))
                       (lambda ([x : ()]) (lambda ([y : ()]) (x y))))))
  (test-false (term (=α (y x) (a b)))))

;;———————————————————————————————————————————————————–———————————————————–——————
;; Typing

(define-extended-language STLC/tc STLC
  (Γ  ::= · (x T Γ)))

(define-environment-helpers (t-lu t-ex) STLC/tc Γ · x T)

(define-judgment-form STLC
  #:mode (⊢_k I O)
  #:contract (⊢_k k B)
  [----------------------- "⊢ₖUnit"
   (⊢_k () ())]
  
  [----------------------- "⊢ₖBool"
   (⊢_k b Bool)]
  
  [----------------------- "⊢ₖInt"
   (⊢_k i Int)])

(define-judgment-form STLC/tc
  #:mode (⊢ I I O)
  #:contract (⊢ Γ e T)
  [(where (some T) (t-lu Γ x))
   ------------------------ "wt-var"
   (⊢ Γ x T)]
  [(⊢_k k B)
   ------------------------ "wt-base"
   (⊢ Γ k B)]
  [(where Γ_new (t-ex Γ x T))
   (⊢ Γ_new e T_e)
   ------------------------ "wt-lambda"
   (⊢ Γ (lambda ([x : T]) e) (T -> T_e))]
  [(where Γ_new (t-ex Γ x T))
   (⊢ Γ_new e_body T_body)
   (⊢ Γ_new f T)
   ------------------------ "wt-letrec"
   (⊢ Γ (letrec ([x : T f]) e_body) T_body)]

  [(where Γ_new (t-ex (t-ex Γ x_1 T_1) x_2 T_2))
   (⊢ Γ_new e_body T_body)
   (⊢ Γ_new f_1 T_1)
   (⊢ Γ_new f_2 T_2)
   ------------------------ "wt-letrec2"
   (⊢ Γ (letrec2 ([x_1 : T_1 f_1] [x_2 : T_2 f_2]) e_body) T_body)]
  
  [(where Γ_new (t-ex Γ x_1 T_1))
   (⊢ Γ_new e_body T_body)
   (⊢ Γ e_1 T_1)
   ------------------------ "wt-let"
   (⊢ Γ (let ([x_1 : T_1 e_1]) e_body) T_body)]
  [(where Γ_new (t-ex (t-ex Γ  x_1 T_1) x_2 T_2))
   (⊢ Γ_new e_body T_body)
   (⊢ Γ e_1 T_1)
   (⊢ Γ e_2 T_2)
   ------------------------ "wt-let2"
   (⊢ Γ (let ([x_1 : T_1 e_1][x_2 : T_2 e_2]) e_body) T_body)]
  [(⊢ Γ e_0 (T_1 -> T_r))
   (⊢ Γ e_1 T_1)
   ------------------------ "wt-app"
   (⊢ Γ (e_0 e_1) T_r)]
  [(where (T_1 T_2 -> T_r) (tyop o))
   (⊢ Γ e_1 T_1)
   (⊢ Γ e_2 T_2)
   ------------------------ "wt-op"
   (⊢ Γ (o e_1 e_2) T_r)]
  [(⊢ Γ e_t Bool)
   (⊢ Γ e_c T)
   (⊢ Γ e_a T)
   ------------------------ "wt-if"
   (⊢ Γ (if e_t e_c e_a) T)])

(define-metafunction STLC
  tyop : o -> (T T -> T)
  [(tyop +) (Int Int -> Int)]
  [(tyop -) (Int Int -> Int)]
  [(tyop *) (Int Int -> Int)]
  [(tyop =) (Int Int -> Bool)])

(define (gen-stlc-term-and-type!)
  (match (generate-term STLC #:satisfying (⊢ · e T) (random 1 5))
    [`(⊢ · ,e ,t) (values e t)]
    [#f (gen-stlc-term-and-type!)]))

(define-term good.0 (+ 1 2))
(define-term good.1 (lambda ([c : Int])
                      (lambda ([a : Int])
                        (+ a c))))
(define-term good.2 (lambda ([a : ()])
                      (lambda ([b : Bool])
                        (lambda ([c : Int])
                          ((if b
                               (lambda ([a : Int])
                                 (+ a c))
                               (lambda ([b : Int])
                                 (- b c)))
                           41)))))
(define-term good ((((lambda ([a : ()])
                             (lambda ([b : Bool])
                               (lambda ([c : Int])
                                 ((if b
                                      (lambda ([a : Int])
                                        (+ a c))
                                      (lambda ([b : Int])
                                        (- b c)))
                                  41))))
                           ())
                          #t)
                         1))


(define-term fact5
  (letrec ([fact : (Int -> Int)
                 (lambda ([x : Int])
                   (if (= x 0)
                       1
                       (* x (fact (- x 1)))))])
    (fact 5)))

(define-term fact3
  (letrec ([fact : (Int -> Int)
                              (lambda ([x : Int])
                                (if (= x 0)
                                    1
                                    (* x (fact (- x 1)))))])
    (fact 3)))

(define-term odd5
  (letrec2 ([even? : (Int -> Bool)
                                    (lambda ([n : Int])
                                      (if (= n 0)
                                          #t
                                          (odd? (- n 1))))]
                             [odd?  : (Int -> Bool)
                                    (lambda ([n : Int])
                                      (if (= n 0)
                                          #t
                                          (even? (- n 1)))) ])
                      (odd? 5)))

(module+ test
  (test-true (redex-match? STLC/tc Γ (term ·)))
  (test-true (redex-match? STLC/tc (lambda ([x : T]) e) fst0))
  (test-true (redex-match? STLC/tc T (term (() -> (() -> ())))))
  (test-true (judgment-holds (⊢ (t-ex · x ()) x ())))
  (test-true (judgment-holds (⊢ · (lambda ([x : ()]) x) (() -> ()))))
  (test-true (judgment-holds (⊢ · (lambda ([x : ()]) (lambda ([x : ()]) x))  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢ · ,fst0  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢ · ,fst1  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢ · ,snd0  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢ · ,snd1  (() -> (() -> ())))))
  (test-true  (judgment-holds (⊢ · good.0 Int)))
  (test-true  (judgment-holds (⊢ · good.1 (Int -> (Int -> Int)))))
  (test-true  (judgment-holds (⊢ · good.2 (() -> (Bool -> (Int -> Int))))))
  (test-true  (judgment-holds (⊢ · good Int)))
  (test-true (judgment-holds (⊢ · fact5 Int)))
  (test-true (judgment-holds (⊢ · odd5 Bool))))

;;---------------------------------------------------------------------
;; The simply typed lambda calculus

(define-extended-language STLC-> STLC
  (v ::= i b () (lambda ([x : T]) e))
  (C ::= hole
         (e ... C e ...)
         (o e ... C e ...)
         (if C e e)
         (if e C e)
         (if e e C)
         (letrec ([x : T C]) e)
         (letrec ([x : T f]) C)
         (letrec2 ([x : T C] [x : T f]) e)
         (letrec2 ([x : T f] [x : T C]) e)
         (letrec2 ([x : T f] [x : T f]) C)
         (let ([x : T C]) e)
         (let ([x : T e]) C)
         (let2 ([x : T C] [x : T e]) e)
         (let2 ([x : T e] [x : T C]) e)
         (let2 ([x : T e] [x : T e]) C)
         (lambda ([x : T]) C)))

(define f0  (term (lambda ([x : ()]) x)))
(define ex1 (term (,f0 ())))
(define f1  (term (lambda ([f : (() -> ())]) (lambda ([x : ()]) (f x)))))
(define ex2 (term ((,f1 ,f0) ,ex1)))
(define ex3 (term ((,f1 (,f1 ,f0)) ,ex1)))

(module+ test
  (test-true (judgment-holds (⊢ · ,ex1 ())))
  (test-true (judgment-holds (⊢ · ,f1 ((() -> ()) -> (() -> ())))))
  (test-true (judgment-holds (⊢ · ,ex2 ())))
  (test-true (judgment-holds (⊢ · ,ex3 ()))))

(define-metafunction STLC->
  δ : o e ... -> e
  [(δ + i_1 i_2) ,(+ (term i_1) (term i_2))]
  [(δ - i_1 i_2) ,(- (term i_1) (term i_2))]
  [(δ * i_1 i_2) ,(* (term i_1) (term i_2))]
  [(δ = i_1 i_2) ,(= (term i_1) (term i_2))])

(define -->β
  (reduction-relation
   STLC->
   (--> (in-hole C ((lambda ([x : _]) e_b) e_p))
        (in-hole C (substitute e_b x e_p))
        β)
   ;; Primitive operators are evaluated using the delta metafuntion
   (--> (in-hole C (o v_1 v_2))
        (in-hole C (δ o v_1 v_2))
        δ)
   ;; Letrecs get unrolled by one loop
   (--> (in-hole C (letrec ([x : T f]) e))
        (in-hole C (substitute e x e_knot))
        (where e_knot (letrec ([x : T f]) f))
        μ)
   (--> (in-hole C (letrec2 ([x_1 : T_1 f_1][x_2 : T_2 f_2]) e))
        (in-hole C (substitute (substitute e x_1 e_knot1) x_2 e_knot2))
        (where e_knot1 (letrec2 ([x_1 : T_1 f_1][x_2 : T_2 f_2]) f_1))
        (where e_knot2 (letrec2 ([x_1 : T_1 f_1][x_2 : T_2 f_2]) f_2))
        μ2)
   ;; Let get unrolled by one loop
   (--> (in-hole C (let ([x : T e]) e_body))
        (in-hole C (substitute e_body x e))
        let)
   (--> (in-hole C (let2 ([x_1 : T_1 e_1][x_2 : T_2 e_2]) e))
        (in-hole C (substitute (substitute e x_1 e_1) x_2 e_2))
        let2)
   ;; Branching conditionals
   (--> (in-hole C (if #t e_c e_a))
        (in-hole C e_c)
        ift)
   (--> (in-hole C (if #f e_c e_a))
        (in-hole C e_a)
        iff)))

(module+ test
  (test-->> -->β #:equiv =α/racket ex1 (term ()))
  (test-->> -->β #:equiv =α/racket ex2 (term ()))
  (test-->> -->β #:equiv =α/racket ex3 (term ())))

(begin-for-syntax
  (define-syntax-rule (with-prefixed-ids p (i ...) b ...)
    (with-syntax ([(i ...) #`(#,(format-id #'p "~a~a" #'p #'i) ...)])
      b ...)))

;; ——————-
;; call by value semantics for the simply typed lambda calculus
(define-syntax (define-stlc-like-reduction-relation stx)
  (syntax-case stx ()
    [(_ s->βv lang var unit int bool fun expr type oper)
     (with-prefixed-ids stlc
       (e e_f e_b e_k e_k1 e_k2 e_c e_a
          v v_1 v_2
          E o f
          x x_b x_f x_1 x_2 x_12 x_22
          T T_b T_f T_1 T_2 T_12 T_22)
       #`(begin
           ))]))

(define-extended-language cbv STLC
  #;(o ::= oper)
  #;(e ::= expr)
  #;(f ::= fun)
  #;(v ::= unit int bool fun)
  (v ::= () i b f)
  #;(x ::= var)
  #;(T ::= type)
  (E ::=
     hole
     (E e)
     (v E)
     (o E e)
     (o v E)
     (if E e e)
     (let ([x : T E]) e)
     (let2 ([x : T E] [x : T e]) e)
     (let2 ([x : T v] [x : T E]) e)))

(define s->βv
  (reduction-relation
   cbv
   ;; Standard Beta for Call by Value 
   (--> (in-hole E ((lambda ([x : T]) e) v))
        (in-hole E (substitute e x v))
        β)
   ;; Primitive operators are evaluated using the delta metafuntion
   (--> (in-hole E (o v_1 v_2))
        (in-hole E (δ o v_1 v_2))
        δ)
   ;; Letrecs get unrolled by one loop
   (--> (in-hole E (letrec ([x_b : T_b (lambda ([x_f : T_f]) e_f)])
                     e_b))
        (in-hole E (substitute e_b x_b e_k))
        (where e_k (letrec ([x_b : T_b (lambda ([x_f : T_f]) e_f)])
                     (lambda ([x_f : T_f]) e_f)))
        μ)
   (--> (in-hole E (letrec2 ([x_1 : T_1 (lambda ([x_12 : T_12]) e_1)]
                             [x_2 : T_2 (lambda ([x_22 : T_22]) e_2)])
                            e_b))
        (in-hole E (substitute (substitute e_b x_1 e_k1) x_2 e_k2))
        (where e_k1 (letrec2 ([x_1 : T_1 (lambda ([x_12 : T_12]) e_1)]
                              [x_2 : T_2 (lambda ([x_22 : T_22]) e_2)])
                             (lambda ([x_12 : T_12]) e_1)))
        (where e_k2 (letrec2 ([x_1 : T_1 (lambda ([x_12 : T_12]) e_1)]
                              [x_2 : T_2 (lambda ([x_22 : T_22]) e_2)])
                             (lambda ([x_22 : T_22]) e_2)))
        μ2)
   ;; Let get unrolled by one loop
   (--> (in-hole E (let ([x : T v]) e_b))
        (in-hole E (substitute e_b x v))
        let)
   (--> (in-hole E (let2 ([x_1 : T_1 v_1][x_2 : T_2 v_2]) e))
        (in-hole E (substitute (substitute e x_1 v_1) x_2 v_2))
        let2)
   ;; Branching conditionals
   (--> (in-hole E (if #t e_c e_a))
        (in-hole E e_c)
        ift)
   (--> (in-hole E (if #f e_c e_a))
        (in-hole E e_a)
        iff)))

#;
(define-stlc-like-reduction-relation s->βv
  STLC v () i b f e T o)

(module+ test
  (add-coverage! s->βv)
  (test-->> s->βv #:equiv =α/racket (term ()) (term ()))
  (test-->> s->βv #:equiv =α/racket (term ((lambda ([x : ()]) ()) ())) (term ()))
  (test-->> s->βv #:equiv =α/racket (term ((lambda ([x : ()]) x) ())) (term ()))
  (test-->> s->βv #:equiv =α/racket ex1 (term ()))
  (test-->> s->βv #:equiv =α/racket ex2 (term ()))
  (test-->> s->βv #:equiv =α/racket ex3 (term ()))
  (test-->> s->βv #:equiv =α/racket (term fact3) 6)
  (test-->> s->βv #:equiv =α/racket (term odd5) #t))


(define-judgment-form STLC
  #:mode (=> I O)
  #:contract (=> e e)
  [(where (e_2) ,(apply-reduction-relation s->βv (term e_1)))
   --------------------
   (=> e_1 e_2)])

(define-judgment-form STLC
  #:mode (progress I)
  #:contract (progress e)
  [(where #f ,(judgment-holds (⊢ · e T)))
   --------------------------------------- "progress e ∉ T " 
   (progress e)]
  [--------------- "progress e=v"
   (progress v)]
  [(=> e_1 e_2) 
   ----------------------------- "progress e->e'"
   (progress e_1)])

(define-judgment-form STLC
  #:mode (preservation I)
  #:contract (preservation e)
  [(where #f ,(judgment-holds (⊢ · e T)))
   --------------------------------------- "preservation e ∈ T " 
   (preservation e)]
  [-------------------- "preservation e -/-> e'"
   (preservation v)]
  [(=> e_1 e_2) (⊢ · e_1 T) (⊢ · e_2 T)
   -------------------- "presevation "
   (preservation e_1)])

(define-judgment-form STLC
  #:mode (sound I)
  #:contract (sound e)
  [(progress e) (preservation e)
   -------------------- ""
   (sound e)])




(module+ test
  (redex-check STLC #:satisfying (⊢ · e t)
               (judgment-holds (sound e))))

