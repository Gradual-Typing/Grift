#lang racket/base

(require redex "stlc.rkt" "./helpers.rkt")

(provide
 ;; The syntax of the gradually typed lambda calculus
 GTLC
 ;; Capture avoiding substitution
 ;; (subst '((xs es) ...) e) : metafunction
 ;; subsitute all xs for es in e renaming as needed to avoid capture
 ;; subst
 ;; Alpha equivalence
 ;; (=α t1 t2) : metafunction
 ;; is t1 alpha equivalent to t2? 
 ;; =α/racket
 ;; (⊢ ((xs Ts) ...) e T) : Judgement (⊢ I I O)
 ;; from the environment, ((xs Ts) ...), we can infer that expression
 ;; e is of type T.
 ⊢_?
 ;; (≈ T G) is T consistent with G
 ≈
 ;; (≤ τ_1 τ_2 τ_3) The meet of τ₁ and τ₂ is τ₃ in the gradual lattice
 ≤ 
  )

(check-redudancy #t)

(define-extended-language GTLC STLC
  ;; Expressions now allow ascription
  (e   ::= .... (: e τ))
  ;; Types now include the dynamic type
  (?   ::= Dyn)
  (τ   ::= .... ?)
  ;; Begin extra patterns for pattern matching
  (fun-fml    ::=  x    (x : τ))
  (let-bnd    ::= (x e) (x : τ e))
  (letrec-bnd ::= (x f) (x : τ f)))

;; λ is just syntax sugar that allows you to omit type annotations
;; if you actually mean dynamic.
(define-metafunction GTLC
  λ : (fun-fml ...) any any ... -> (lambda ([x : τ] ...) any)
  [(λ ((x_1 : τ_1) ... x any_2 ...) any ...)
   (λ ((x_1 : τ_1) ... (x : Dyn) any_2 ...) any ...)]
  [(λ ((x : τ) ...) any_e)
   (lambda ((x : τ) ...) any_e)]
  [(λ ((x : τ_a) ...) : τ_r any_e)
   (lambda ((x : τ_a) ...) (: any_e τ_r))])


(module+ test
  (test-true (term (=α (λ (x) x) (lambda ([x : Dyn]) x))))
  (test-true (term (=α (λ ([x : Int]) : Int x)
                       (lambda ([x : Int]) (: x Int)))))
  (test-true (term (=α (λ (x [y : ()] z) y)
                       (lambda ([x : Dyn] [y : ()] [z : Dyn]) y)))))

;; return τ is the expression is an ascription otherwise Dyn
(define-metafunction GTLC
  ascription-or-dyn : e -> τ
  [(ascription-or-dyn (: _ τ)) τ]
  [(ascription-or-dyn _)       Dyn])

;; Don't stack consistent ascriptions 
(define-metafunction GTLC
  ascribe : e τ -> e
  [(ascribe (: e τ_1) τ_2) (: e τ_2)
   (side-condition (term (compat τ_1 τ_2)))]
  [(ascribe e τ) (: e τ)])

;; Give the least precise compatable type that is more precise
;; than either type given. IE greatest lower bound
(define-metafunction GTLC
  meet : τ τ -> τ
  [(meet ? τ) τ]
  [(meet τ ?) τ]
  [(meet τ τ) τ]
  [(meet (τ_1a ..._n -> τ_1r)
         (τ_2a ..._n -> τ_2r))
   ((meet τ_1a τ_2a) ... -> (meet τ_1r τ_2r))])

(define (id x) x)

(define-metafunction GTLC
  compat : τ τ -> boolean
  [(compat τ τ) #t]
  [(compat τ ?) #t]
  [(compat ? τ) #t]
  [(compat (τ_1a ..._n -> τ_1r) (τ_2a ..._n -> τ_2r))
   ,(andmap id (term ((compat τ_1r τ_2r) (compat τ_1a τ_2) ...)))]
  [(compat _ _) #f])

;; ltr-fun->type build a function type from the type annotations
;; on a lambda.
(define-metafunction GTLC
  ltr-fun->type : f -> (τ ... -> τ)
  [(ltr-fun->type (lambda ([x : τ_a] ...) (: e τ_r))) (τ_a ... -> τ_r)]
  [(ltr-fun->type (lambda ([x : τ_a] ...) e)) (τ_a ... -> Dyn)])

(define-metafunction GTLC
  ltr-meet-type : x τ f -> [x : τ f]
  [(ltr-meet-type x (τ_1a ... -> τ_1r) (lambda ([x_2 : τ_2a] ...) e))
   [x : (τ_3a ... -> τ_3r)
      (lambda ([x_2 : τ_3a] ...)
        (ascribe e τ_3r))]
   (where τ_2r (ascription-or-dyn e))
   (where (τ_3a ... -> τ_3r) (meet (τ_1a ... -> τ_1r) (τ_2a ... -> τ_2r)))]
  ;; If inconsistent or so such just leave the types alone
  [(ltr-meet-type any_x any_t any_f) [any_x : any_t any_f]])

;; ltr is just syntax sugar that allows you to omit type annotations
;; in letrecs so that you do not need to duplicate annotations.
;; It uses the most precise annotation that can be derived by meeting
;; the annotations included in both the variable annotation and the
;; lambda on the right hand side.
(define-metafunction GTLC
  ltr : (letrec-bnd ...) e -> (letrec ([x : τ f] ...) e)
  [(ltr ([x_1 : τ_1 f_1] ... [x f] any_2 ...) e)
   (ltr ([x_1 : τ_1 f_1] ...
         [x : (ltr-fun->type f) f]
         any_2 ...)
    e)]
  [(ltr ([x : τ f] ...) e)
   (letrec ([ltr-meet-type x τ f] ...)  e)])

(define ns0 (term (ltr ([f (λ (x) x)]) (f f))))
(define ns1
  (term (ltr ([f (λ ([x : Int] [y : Int]) : Bool (= x y))]
              [l (λ ([x : Int]) : Int
                    (if (f x x)
                        (l (- x 1))
                        0))])
             (l 10))))
(define ns2 (term (ltr ([x : (Int -> Dyn) (λ (x) : Int x)]) x)))

(module+ test
  (test-true (term (=α ,ns0 (letrec ([f : (Dyn -> Dyn) (λ (x) (: x Dyn))]) (f f)))))
  (test-true (redex-match? GTLC e ns1))
  (test-true (redex-match? GTLC e ns2)))

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

(module+ test
  (test-true (redex-match? GTLC e fst0))
  (test-true (redex-match? GTLC e fst1))
  (test-true (redex-match? GTLC e fst10))
  (test-true (redex-match? GTLC e fst11))
  (test-true (redex-match? GTLC e snd0))
  (test-true (redex-match? GTLC e snd1))
  (test-true (redex-match? GTLC e snd11))
  (test-true (redex-match? GTLC e snd10)))

;;———————————————————————————————————————————————————–———————————————————–——————
;; Scope helpers

(define-metafunction GTLC
  in : any (any ...) -> boolean
  [(in any_1 (any_n ... any_1 any_m ...)) #t]
  [(in _ _) #f])

(require racket/set)

;; fvs : Collect the unique free variables of a gradual expression
(define-metafunction GTLC
  fvs : any -> (x_!_ ...)
  [(fvs x) (x)]
  [(fvs (lambda ([x : _] ...) any))
   ,(set-subtract (term (fvs any)) (term (x ...)))]
  [(fvs (any ...))
   ,(foldr set-union (term ()) (term ((fvs any) ...)))])

;; fv? : Does a variable occur free in a expression?
(define-metafunction GTLC
  fv? : x any -> boolean
  [(fv? x any) (in x (fvs any))])

(module+ test
  (test-true  (term (fv? x ,xfree0)))
  (test-true  (term (fv? x ,xfree1)))
  (test-false (term (fv? x ,x!free0)))
  (test-false (term (fv? x ,x!free1))))

;; bvs : return the bound variables of an expression
(define-metafunction GTLC
  bvs : any -> (x_!_ ...)
  [(bvs x) ()]
  [(bvs (lambda ([x : _] ...) any))
   ,(set-union (set-intersect (term (fvs any)) (term (x ...)))
               (term (bvs any)))]
  [(bvs (any ...)) ,(foldr set-union (term ()) (term ((bvs any) ...)))])

;; bv? : test if a variable occurs bound in an expression
(define-metafunction GTLC
  bv? : x any -> boolean
  [(bv? x any) (in x (bvs any))])

(module+ test
  (test-true  (term (bv? x ,xbound0)))
  (test-true  (term (bv? x ,xbound1)))
  (test-false (term (bv? x ,x!bound0)))
  (test-false (term (bv? x ,x!bound1))))

;; =α : are two expression alpha equivalent?
#;
(define-metafunction GTLC
  =α : any any -> boolean
  [(=α any_1 any_2) ,(equal? (term (sd any_1))
                             (term (sd any_2)))])

#;(define (=α/racket t1 t2) (term (=α ,t1 ,t2)))

;; sd convert regular bindings
#;
  (define-extended-language GTLCΔ GTLC
    (T ::= any)
    (e ::= any)
    (m ::= e (K n n)
       (letrec ([T l] ...) m)
       (let ([T m] ...) m))
    (l ::= (lambda (T ...) m))
    (n ::= natural)
    (Γ ::= ((x ...) ...)))

#;
  (define-metafunction GTLCΔ
    sd : any -> any
    [(sd any) (sd/env any ())])

#;
  (define-metafunction GTLCΔ
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
    [(sd/env (letrec ([x : τ l] ...) e) (any_1 ...))
     (letrec ([T (sd/env l ((x ...) any_1 ...))] ...)
       (sd/env e ((x ...) any_1 ...)))]
    [(sd/env (let ([x : T m_rhs] ...) m_body) (any_1 ...))
     (let([T (sd/env m_rhs ((x ...) any_1 ...))] ...)
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

#;
(define-metafunction GTLC
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
  [(subst _ any) any #;,(error 'subst "unmatched term ~a" (term any)) ;
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


#;
(define-metafunction GTLC
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

(define-metafunction GTLC
  lookup : ((any any) ...) any -> (none) or (some any)
  [(lookup () _) (none)]
  [(lookup ((any_a any_d) (any_a* any_d*) ...) any_a) (some any_d)]
  [(lookup ((_ _) (any_a* any_d*) ...) any)
   (lookup ((any_a* any_d*) ...) any)])

(module+ test
  (test-equal (term (lookup ((x 1) (y 2) (z 3)) x)) (term (some 1)))
  (test-equal (term (lookup ((x 1) (y 2) (z 3)) y)) (term (some 2)))
  (test-equal (term (lookup ((x 1) (y 2) (z 3)) z)) (term (some 3)))
  (test-equal (term (lookup ((x 1) (y 2) (z 3)) a)) (term (none))))

(define-metafunction GTLC
  extend : ((any any) ...) (any any) ... -> ((any any) ...)
  [(extend (any ...) (any_a any_d) ...) ((any_a any_d) ... any ...)])

;;———————————————————————————————————————————————————–———————————————————–——————
;; Typing

(define-extended-language GTLC/TC GTLC
  (Γ  ::= ((x τ) ...)))

(define-judgment-form GTLC/TC
  #:mode (≈ I I)
  #:contract (≈ τ τ)

  [------------------------ "≈Refl"
            (≈ τ τ)]

  [------------------------ "≈Right"
            (≈ ? τ)]
  [------------------------ "≈Left"
            (≈ τ ?)]

  [(≈ τ_1a τ_2a) ... (≈ τ_1r τ_2r)
   ---------------------------------------- "≈Cong"
   (≈ (τ_1a ..._n -> τ_1r) (τ_2a ..._n -> τ_2r))])


(define-judgment-form GTLC/TC
  #:mode (≤ I I O)
  #:contract (≤ τ τ τ)

  [------------------------ "≤Refl"
   (≤ τ τ τ)]

  [------------------------ "≤Right"
   (≤ ? τ τ)]
  [------------------------ "≤Left"
   (≤ τ ? τ)]

  [(≤ τ_1a τ_2a τ_3a) ... (≤ τ_1r τ_2r τ_3r)
   ---------------------------------------- "≤Cong"
   (≤ (τ_1a ..._n -> τ_1r) (τ_2a ..._n -> τ_2r) (τ_3a ... -> τ_3r))])

(define-judgment-form GTLC/TC
  #:mode (⊢_? I I O)
  #:contract (⊢_? Γ e τ)
  [(where (some τ) (lookup Γ x))
   ------------------------ "var"
          (⊢_? Γ x τ)]

  [------------------------ "⊢_?()"
          (⊢_? Γ () ())]

  [------------------------ "⊢_?Int"
          (⊢_? Γ i Int)]

  [------------------------ "⊢_?Bool"
          (⊢_? Γ b Bool)]
  
  [(where Γ_ν (extend Γ (x_a τ_a) ...)) (⊢_? Γ_ν e τ_r)
   ------------------------------------------------------- "⊢_?lam"
   (⊢_? Γ (lambda ([x_a : τ_a] ...) e) (τ_a ... -> τ_r))]

  [(where Γ_ν (extend Γ (x_b τ_b) ...))
   (⊢_? Γ_ν f_b τ_b) ... (⊢_? Γ_ν e τ)
   ----------------------------------- "⊢_?letrec"
   (⊢_? Γ (letrec ([x_b : τ_b f_b] ...) e) τ)]
  
  [(where Γ_b (extend Γ (x_b τ_b) ...))
   (⊢_? Γ e_b τ_e) ... (≈ τ_b τ_e) ... (⊢_? Γ_b e τ)
   --------------------------------------- "⊢_?let"
   (⊢_? Γ (let ([x_b : τ_b e_b] ...) e) τ)]

  [(⊢_? Γ e_f (τ_1a ..._n -> τ_r)) (⊢_? Γ e_a τ_2a) ... (≈ τ_1a τ_2a) ...
   -------------------------------------------------------------- "⊢_?app-fun"
   (⊢_? Γ (e_f e_a ..._n) τ_r)]

  [(⊢_? Γ e_0 ?) (⊢_? Γ e τ) ...
   -------------------------------"⊢_?app-?"
   (⊢_? Γ (e_0 e ..._n) Dyn)]
    
  [(where (τ_a ..._n -> τ_r) (tyop o))
   (⊢_? Γ e_a τ_a) ...
   ------------------------ "⊢_?app-op"
   (⊢_? Γ (o e_a ..._n) τ_r)]
  [(⊢_? Γ e_t τ_t) (≈ τ_t Bool)
   (⊢_? Γ e_c τ_c) (⊢_? Γ e_a τ_a) (≤ τ_c τ_a τ_meet)
   ------------------------ "⊢_?if"
   (⊢_? Γ (if e_t e_c e_a) τ_meet)])

(define-metafunction GTLC
  tyop : o -> any
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

(define good1 (term ((if #t
                          (lambda ([a : Int])
                            ((lambda () (+ a 0))))
                          (lambda ([b : Int])
                            (- b 84)))
                     42)))



(define good2 (term ((lambda ([a : ()] [b : Bool] [c : Int])
                       c)
                     () #t 0)))

(define good3 (term ((lambda ([a : Dyn]) ((lambda ([a : Int]) (+ a a)) 2)) #f)))

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
  (test-true (redex-match? GTLC/TC Γ '()))
  (test-true (redex-match? GTLC/TC (lambda ([x_m : τ_n] ...) e) fst0))
  (test-true (redex-match? GTLC/TC τ (term (() -> (() -> ())))))
  (test-true (judgment-holds (⊢_? ((x ())) x ())))
  (test-true (judgment-holds (⊢_? () ,fst0  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢_? () ,fst1  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢_? () ,fst10 (() () -> ()))))
  (test-true (judgment-holds (⊢_? () ,fst11 (() () -> ()))))
  (test-true (judgment-holds (⊢_? () ,snd0  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢_? () ,snd1  (() -> (() -> ())))))
  (test-true (judgment-holds (⊢_? () ,snd10 (() () -> ()))))
  (redex-let GTLC ([e_g  good]
                   [e_g1 good1]
                   [e_g2 good2]
                   [e_g3 good3])
             (test-true  (judgment-holds (⊢_? () e_g Int)))
             (test-true  (judgment-holds (⊢_? () e_g1 Int)))
             (test-true  (judgment-holds (⊢_? () e_g2 Int)))
             (test-true  (judgment-holds (⊢_? () e_g3 Int))))
  
  (test-false (judgment-holds (⊢_? () ,bad Int)))
  (redex-let GTLC ([e_f5 fact5]
                [e_o5 odd5])
    (test-true (judgment-holds (⊢_? () e_f5 Int)))
    (test-true (judgment-holds (⊢_? () e_o5 Bool)))))

;; TODO I could show at this point that all well typed stlc programs
;; are well typed in gtlc.

;; TODO I could show that any well typed program made more dynamic
;; is also well typed.

(module+ test (test-results))


