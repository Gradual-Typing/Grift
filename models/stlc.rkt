#lang racket

(require redex
         "helpers.rkt"
         "base.rkt")

(module+ test
  ;; Make sure that we test the entire stack of languages
  (require (submod "base.rkt" test)))

(provide
 ;; The syntax of the simply typeded lambda calculus
 STLC STLC/tc
 ;; Capture avoiding substitution
 ;; (subst '((xs es) ...) e) : metafunction
 ;; subsitute all xs for es in e renaming as needed to avoid capture
 subst
 ;; Alpha equivalence
 ;; (=α t1 t2) : metafunction
 ;; is t1 alpha equivalent to t2? 
 =α
 ;; (=α/racket t1 t2) racket function
 ;; is t1 alpha equivalent to t2? 
 =α/racket
 ;; (⊢ ((xs τs) ...) e τ) : Judgement (⊢ I I O)
 ;; from the environment, ((xs τs) ...), we can infer that expression
 ;; e is of type τ.
 ⊢
 ;; Reduction relation for the calculus
 -->β
 ;; Standard reduction relation for the call-by value semantics
 s->βv
 ;; (tyop o) what is the type of o.
 tyop)

(define-extended-language STLC BASE
  ;; Things that should definately not be included in the grammar
  ;; if produced as a pic or pdf
  (x ::= variable-not-otherwise-mentioned)
  ;; End things that must not be named 
  (e   ::= k x f
           (letrec ([x_!_ : τ f] ...) e)
           (let ([x_!_ : τ e] ...) e)
           (o e ...)
           (e e ...)
           (if e e e))
  (f ::= (lambda ([x_!_ : τ] ...) e))
  ;;
  (o ::= + - * =)
  ;; Types
  (τ ::= ι (τ ... -> τ)))

;;———————————————————————————————————————————————————–———————————————————–——————
;; Scope helpers

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
  (define fst10 (term (lambda ([x : ()] [y : ()]) x)))
  (define fst11 (term (lambda ([n : ()] [m : ()]) n)))
  (define snd0  (term (lambda ([x : ()]) (lambda ([y : ()]) y))))
  (define snd1  (term (lambda ([n : ()]) (lambda ([m : ()]) m))))
  (define snd10 (term (lambda ([x : ()] [y : ()]) y)))
  (define snd11 (term (lambda ([n : ()] [m : ()]) m)))

  (test-true (redex-match? STLC e fst0))
  (test-true (redex-match? STLC e fst1))
  (test-true (redex-match? STLC e fst10))
  (test-true (redex-match? STLC e fst11))
  (test-true (redex-match? STLC e snd0))
  (test-true (redex-match? STLC e snd1))
  (test-true (redex-match? STLC e snd11))
  (test-true (redex-match? STLC e snd10))
  )


;; fvs : Collect the unique free variables of a gradual expression
(define-metafunction STLC
  fvs : any -> (x_!_ ...)
  [(fvs x) (x)]
  [(fvs (lambda ([x : _] ...) any))
   ,(set-subtract (term (fvs any)) (term (x ...)))]
  [(fvs (any ...))
   ,(foldr set-union (term ()) (term ((fvs any) ...)))])

;; fv? : Does a variable occur free in a expression?
(define-metafunction STLC
  fv? : x any -> boolean
  [(fv? x any) (in x (fvs any))])

(module+ test
  (test-true  (term (fv? x ,xfree0)))
  (test-true  (term (fv? x ,xfree1)))
  (test-false (term (fv? x ,x!free0)))
  (test-false (term (fv? x ,x!free1))))

;; bvs : return the bound variables of an expression
(define-metafunction STLC
  bvs : any -> (x_!_ ...)
  [(bvs x) ()]
  [(bvs (lambda ([x : _] ...) any))
   ,(set-union (set-intersect (term (fvs any)) (term (x ...)))
               (term (bvs any)))]
  [(bvs (any ...)) ,(foldr set-union (term ()) (term ((bvs any) ...)))])

;; bv? : test if a variable occurs bound in an expression
(define-metafunction STLC
  bv? : x any -> boolean
  [(bv? x any) (in x (bvs any))])

(module+ test
  (test-true  (term (bv? x ,xbound0)))
  (test-true  (term (bv? x ,xbound1)))
  (test-false (term (bv? x ,x!bound0)))
  (test-false (term (bv? x ,x!bound1)))
  (get-coverage))


;; =α : are two expression alpha equivalent?
(define-metafunction STLC
  =α : any any -> boolean
  [(=α any_1 any_2) ,(equal? (term (sd any_1))
                             (term (sd any_2)))])

(define (=α/racket t1 t2) (term (=α ,t1 ,t2)))

;; sd convert regular bindings
(define-extended-language STLCΔ STLC
  (τ ::= any)
  (e ::= any)
  (m ::= e (K n n)
         (letrec ([τ f] ...) m)
         (let ([τ m] ...) m))
  (f ::= (lambda (τ ...) m))
  (n ::= natural)
  (Γ ::= ((x ...) ...)))

(define-metafunction STLCΔ
  sd : any -> any
  [(sd any) (sd/env any ())])

(define-metafunction STLCΔ
  sd/env : m Γ -> m
  [(sd/env x ((x_1 ...) ...
              (x_2 ... x x_3 ...)
              (x_4 ...) ...))
   (K ,(length (term ((x_1 ...) ...)))
      ,(length (term (x_2 ...))))
   (where #f (in x (x_1 ... ...)))]
  [(sd/env x _) x]
  [(sd/env (lambda ([x_n : τ_n] ...) m) ((x ...) ...))
   (lambda (τ_n ...) (sd/env m ((x_n ...) (x ...) ...)))]
  [(sd/env (letrec ([x : τ f] ...) e) (any_1 ...))
   (letrec ([τ (sd/env f ((x ...) any_1 ...))] ...)
     (sd/env e ((x ...) any_1 ...)))]
  [(sd/env (let ([x : τ m_rhs] ...) m_body) (any_1 ...))
   (let([τ (sd/env m_rhs ((x ...) any_1 ...))] ...)
     (sd/env m_body ((x ...) any_1 ...)))]
  [(sd/env (m ...) Γ) ((sd/env m Γ) ...)]
  [(sd/env e Γ) e])

(module+ test
  (test-true (term (=α (lambda ([x : ()]) x)
                       (lambda ([y : ()]) y))))
  (test-false (term (=α (lambda ([x : ()]) y)
                        (lambda ([y : ()]) y))))
  (test-true (term (=α (lambda ([x : ()]) x)
                       (lambda ([x : ()]) x))))
  (test-false (term (=α (lambda ([a : ()] [b : ()]) (a b))
                        (lambda ([a : ()] [b : ()])
                          ((lambda ([a : ()] [b : ()]) (a b)) a b)))))
  (test-true (term (=α (lambda ([x : ()]) (lambda ([y : ()]) (x y)))
                       (lambda ([x : ()]) (lambda ([y : ()]) (x y))))))
  (test-false (term (=α (y x) (a b)))))

(define-metafunction STLC
  subst : ((x any) ...) any -> any
  [(subst ((x_0 any_0) ... (x any) (x_1 any_1) ...) x) any]
  [(subst ((x_0 any_0) ...) x) x]
  [(subst ((x_0 any_0) ...) (lambda ([x_1 : any_t] ...) any_e))
   (lambda ([x_new : any_t] ...)
     (subst ((x_0 any_0) ...) (subst-raw ((x_1 x_new) ...) any_e)))
   (where (x_new ...) ,(variables-not-in (term (any_e x_0 ... any_0 ...)) (term (x_1 ...))))
   ]
  [(subst ((x_0 any_0) ...) (letrec ([x_1 : any_t any_l] ...) any_e))
   (letrec ([x_new : any_t
             (subst ((x_0 any_0) ...) (subst-raw ((x_1 x_new) ...) any_l))] ...)
     (subst ((x_0 any_0) ...)
            (subst-raw ((x_1 x_new) ...) any_e)))
   (where (x_new ...) ,(variables-not-in (term (any_e x_0 ... any_l ... any_0 ...)) (term (x_1 ...))))]
  [(subst ((x_0 any_0) ...) (let ([x_1 : any_t any_rhs] ...) any_e))
   (let ([x_new : any_t (subst ((x_0 any_0) ...) any_rhs)] ...)
     (subst ((x_0 any_0) ...) (subst-raw ((x_1 x_new) ...) any_e)))
   (where (x_new ...) ,(variables-not-in (term (any_e x_0 ... any_0 ...)) (term (x_1 ...))))]
  [(subst [(x_0 any_0) ...] (any ...)) ((subst [(x_0 any_0) ...]  any) ...)]
  [(subst _ any) any #;,(error 'subst "unmatched term ~a" (term any))
   ])

(module+ test
  (test-true (term (=α (subst ((x (lambda ([x : Int]) y)))
                              (lambda ([y : Int]) x))
                       (lambda ([y1 : Int]) (lambda ([x : Int]) y)))))
  (test-true (term (=α (subst ((x (lambda ([a : Int]) (x a)))
                               (y (lambda ([a : Int]) (y a))))
                              (letrec ([x : (Int -> Int)
                                          (lambda ([a : Int])
                                            (z a))]
                                       [z : (Int -> Int)
                                          (lambda ([a : Int])
                                            (x (y a)))])
                                (x (z (y 1)))))
                       (letrec ([x1 : (Int -> Int)
                                    (lambda ([a1 : Int])
                                           (z1 a1))]
                                [z1 : (Int -> Int)
                                    (lambda ([a1 : Int])
                                      (x1 ((lambda ([a : Int])
                                             (y a)) a1)))])
                         (x1 (z1 ((lambda ([a : Int]) (y a)) 1))))))))

(define-environment-helpers STLC lookup extend)

(module+ test
  (test-equal (term (lookup ((x 1) (y 2) (z 3)) x)) (term (some 1)))
  (test-equal (term (lookup ((x 1) (y 2) (z 3)) y)) (term (some 2)))
  (test-equal (term (lookup ((x 1) (y 2) (z 3)) z)) (term (some 3)))
  (test-equal (term (lookup ((x 1) (y 2) (z 3)) a)) (term (none))))

(define-metafunction STLC
  subst-raw : ((x x) ...) any -> any
  [(subst-raw ((x_o1 x_n1) ... (x_o x_n) (x_o2 x_n2) ...) x_o) x_n]
  [(subst-raw ((x_o1 x_n1) ...) x) x]
  [(subst-raw ((x_o1 x_n1) ...) (any ...))
   ((subst-raw ((x_o1 x_n1) ...) any) ...)]
  [(subst-raw ((x_o1 x_n1) ...) any) any]
  [(subst-raw ((x_o1 x_n1) ...) (lambda (any_1 ...) any))
   (lambda (any_1 ...) (subst-raw ((x_o1 x_n1) ...) any))])

(module+ test
  (test-equal
   (term (subst ((x1 5)) (if (= x1 0)
                             1
                             (* x1 ((letrec ((fact : (Int -> Int)
                                                   (lambda ((x : Int))
                                                     (if (= x 0)
                                                         1
                                                         (* x (fact (- x 1)))))))
                                      (lambda ((x : Int))
                                        (if (= x 0)
                                            1
                                            (* (fact (- x 1))))))
                                    (- x1 1))))))
   (term (if (= 5 0)
             1
             (* 5 ((letrec ((fact : (Int -> Int)
                                  (lambda ((x : Int))
                                    (if (= x 0)
                                        1
                                        (* x (fact (- x 1)))))))
                     (lambda ((x : Int))
                       (if (= x 0)
                           1
                           (* (fact (- x 1))))))
                   (- 5 1)))))
    #:equiv =α/racket))


(module+ test
  (test-true (term (=α ,fst0 (subst ((x a) (y b)) ,fst0)))))

;;———————————————————————————————————————————————————–———————————————————–——————
;; Typing

(define-extended-language STLC/tc STLC
  (Γ  ::= ((x any) ...)))

(define-judgment-form STLC/tc
  #:mode (⊢ I I O)
  ;;#:contract (⊢ Γ e τ)
  [(where (some τ) (lookup Γ x))
   ------------------------ "λvar"
   (⊢ Γ x τ)]
  [(⊢_k k ι)
   ------------------------ "⊢base"
   (⊢ Γ k ι)]
  [(where Γ_new (extend Γ ((x_m τ_n) ...)))
   (⊢ Γ_new e τ_r)
   ------------------------ "λlam"
   (⊢ Γ (lambda ([x_m : τ_n] ...) e) (τ_n ... -> τ_r))]

  [(where Γ_new (extend Γ ((x_n τ_n) ...)))
   (⊢ Γ_new e_body τ_body)
   (⊢ Γ_new f_n τ_n) ...
   ------------------------ "λletrec"
   (⊢ Γ (letrec ([x_n : τ_n f_n] ...) e_body) τ_body)]
  
  [(where Γ_new (extend Γ ((x_n τ_n) ...)))
   (⊢ Γ_new e_body τ_body)
   (⊢ Γ e_n τ_n) ...
   ------------------------ "λlet"
   (⊢ Γ (let ([x_n : τ_n e_n] ...) e_body) τ_body)]

  [(⊢ Γ e_0 (τ_1 ..._n -> τ_r))
   (⊢ Γ e_1 τ_1) ...
   ------------------------ "λapp"
   (⊢ Γ (e_0 e_1 ..._n) τ_r)]
  [(where (τ ..._n -> τ_r) (tyop o))
   (⊢ Γ e τ) ...
   ------------------------ "λop"
   (⊢ Γ (o e ..._n) τ_r)]
  [(⊢ Γ e_t Bool) (⊢ Γ e_c τ) (⊢ Γ e_a τ)
   ------------------------ "λif"
   (⊢ Γ (if e_t e_c e_a) τ)])

(define-metafunction STLC
  tyop : o -> τ
  [(tyop +) (Int Int -> Int)]
  [(tyop -) (Int Int -> Int)]
  [(tyop *) (Int Int -> Int)]
  [(tyop =) (Int Int -> Bool)])

(define good (term ((lambda ([a : ()] [b : Bool] [c : Int])
                      ((if b
                          (lambda ([a : Int])
                            ((lambda () (+ a c))))
                          (lambda ([b : Int])
                            (- b c)))
                       42))
                    () #t 0)))

(define bad (term ((lambda ([a : ()] [b : Bool] [c : Int])
                      ((if b
                          (lambda ([a : Int])
                            ((lambda () (+ a c))))
                          (lambda ([b : Int])
                            (- b c)))
                       #f))
                    () #t 0)))

(define fact5 (term (letrec ([fact : (Int -> Int)
                              (lambda ([x : Int])
                                (if (= x 0)
                                    1
                                    (* x (fact (- x 1)))))])
                      (fact 5))))

(define fact3 (term (letrec ([fact : (Int -> Int)
                              (lambda ([x : Int])
                                (if (= x 0)
                                    1
                                    (* x (fact (- x 1)))))])
                      (fact 3))))

(define odd5 (term (letrec ([even? : (Int -> Bool)
                             (lambda ([n : Int])
                               (if (= n 0)
                                   #t
                                   (odd? (- n 1))))]
                            [odd?  : (Int -> Bool)
                             (lambda ([n : Int])
                               (if (= n 0)
                                   #t
                                   (even? (- n 1)))) ])
                      (odd? 5))))

(module+ test

  (test-true (redex-match? STLC/tc Γ '()))
  (test-true (redex-match? STLC/tc (lambda ([x_m : τ_n] ...) e) fst0))
  (test-true (redex-match? STLC/tc τ (term (() -> (() -> ())))))
  (test-true (judgment-holds (⊢ ((x ())) x ())))
  (test-true (judgment-holds (⊢ () ,fst0  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢ () ,fst1  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢ () ,fst10 (() () -> ()))))
  (test-true (judgment-holds (⊢ () ,fst11 (() () -> ()))))
  (test-true (judgment-holds (⊢ () ,snd0  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢ () ,snd1  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢ () ,snd10 (() () -> ()))))
  (test-true  (judgment-holds (⊢ () ,good Int)))
  (test-false (judgment-holds (⊢ () ,bad Int)))
  (redex-let STLC ([e_f5 fact5]
                   [e_o5 odd5])
   (test-true (judgment-holds (⊢ () e_f5 Int)))
   (test-true (judgment-holds (⊢ () e_o5 Bool)))))

;;---------------------------------------------------------------------
;; The simply typed lambda calculus

(define-extended-language STLC-> STLC
  (v ::= i b () (lambda ([x_!_ : τ] ...) e))
  (C ::= hole
         (e ... C e ...)
         (o e ... C e ...)
         (if C e e)
         (if e C e)
         (if e e C)
         (letrec ([x_1 : τ_1 f_1] ...
                  [x : τ (lambda ([x_l : τ_l]) C)]
                  [x_2 : τ_2 f_2] ...)
           e)
         (letrec ([x : τ l] ...) C)
         (let ([x_1 : τ_1 e_1] ... [x : τ C] [x_2 : τ_2 e_1] ...) e)
         (let ([x : τ e] ...) C)
         (lambda ((x : τ) ...) C)))

(define f0  (term (lambda ([x : ()]) x)))
(define ex1 (term (,f0 ())))
(define f1  (term (lambda ([f : (() -> ())]) (lambda ([x : ()]) (f x)))))
(define ex2 (term ((,f1 ,f0) ,ex1)))
(define ex3 (term ((,f1 (,f1 ,f0)) ,ex1)))

(module+ test

  (test-true (judgment-holds (⊢ () ,ex1 ())))
  (test-true (judgment-holds (⊢ () ,f1 ((() -> ()) -> (() -> ())))))
  (test-true (judgment-holds (⊢ () ,ex2 ())))
  (test-true (judgment-holds (⊢ () ,ex3 ()))))

(define-metafunction STLC->
  δ : o e ... -> e
  [(δ + i_1 i_2) ,(+ (term i_1) (term i_2))]
  [(δ - i_1 i_2) ,(- (term i_1) (term i_2))]
  [(δ * i_1 i_2) ,(* (term i_1) (term i_2))]
  [(δ = i_1 i_2) ,(= (term i_1) (term i_2))])

(define -->β
  (reduction-relation
   STLC->
   (--> (in-hole C ((lambda ([x_0 : _] ..._n) e) e_n ..._n))
        (in-hole C (subst ([x_0 e_n] ...) e))
        β)
   ;; Primitive operators are evaluated using the delta metafuntion
   (--> (in-hole C (o v ...))
        (in-hole C (δ o v ...))
        δ)
   ;; Letrecs get unrolled by one loop
   (--> (in-hole C (letrec ([x : τ f] ...) e))
        (in-hole C (subst ([x (letrec ([x : τ f] ...) f)] ...) e))
        μ)
   ;; Branching conditionals
   (--> (in-hole C (if #t e_c e_a))
        (in-hole C e_c)
        ift)
   (--> (in-hole C (if #f e_c e_a))
        (in-hole C e_a)
        iff)))


(module+ test
  ;; Way Too long to run every time
  ;;(test-->>∃ #:steps 10 -->β fact3 6)
  #|
  (add-coverage! -->β)
  
  (test-->>∃ #:steps 20 -->β odd5 #t)
  (traces -->β odd5)
  |#
  (test-->> -->β #:equiv =α/racket ex1 (term ()))
  (test-->> -->β #:equiv =α/racket ex2 (term ()))
  (test-->> -->β #:equiv =α/racket ex3 (term ()))

  )

;; ——————-
;; call by value semantics for the simply typed lambda calculus

(define-extended-language STLC->cbv STLC->
  (v ::= () i b f)
  (E ::= hole (v ... E e ...) (if E e e) (o v ... E e ...)))

(define s->βv
  (reduction-relation
   STLC->cbv
   ;; Standard Beta for Call by Value 
   (--> (in-hole E ((lambda ([x : τ] ..._n) e) v ..._n))
        (in-hole E (subst ((x v) ...) e))
        β)
   ;; Primitive operators are evaluated using the delta metafuntion
   (--> (in-hole E (o v ...))
        (in-hole E (δ o v ...))
        δ)
   ;; Letrecs get unrolled by one loop
   (--> (in-hole E (letrec ([x : (τ_1 ...n -> τ_2)
                               (lambda ([x_a : τ_1] ...n) e_f)] ...)
                     e_b))
        (in-hole E (subst ([x (letrec ([x : (τ_1 ... -> τ_2)
                                          (lambda ([x_a : τ_1] ...) e_f)] ...)
                                (lambda ([x_a : τ_1] ...) e_f))] ...)
                          e_b))
        μ)
   ;; Branching conditionals
   (--> (in-hole E (if #t e_c e_a))
        (in-hole E e_c)
        ift)
   (--> (in-hole E (if #f e_c e_a))
        (in-hole E e_a)
        iff)
   ))

(module+ test
  (add-coverage! s->βv)

  (test-->> s->βv #:equiv =α/racket ex1 (term ()))
  (test-->> s->βv #:equiv =α/racket ex2 (term ()))
  (test-->> s->βv #:equiv =α/racket ex3 (term ()))
  (test-->> s->βv #:equiv =α/racket fact3 6)
  (test-->> s->βv #:equiv =α/racket odd5 #t))



       

