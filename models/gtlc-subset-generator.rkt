#lang racket/base

(require redex "gtlc.rkt" "helpers.rkt")

(provide Estimate-GTLC wt)

;; Since languages and rules must be free of ellipsis, unquote, and contexts
;; We are doing a more verbose estimate of the original language
(define-language Estimate-GTLC
  ;; Expression
  (e   ::= app (if e e e) (: e τ) b i x f lt ltr)
  (lt ::=
      (let () e)
      (let ([x : τ e]) e)
      (let ([x_!_ : τ e]
            [x_!_ : τ e])
        e)
      (let ([x_!_ : τ e]
            [x_!_ : τ e]
            [x_!_ : τ e])
        e))
  (ltr ::=
       (letrec () e)
       (letrec ([x : τ f]) e)
       (letrec ([x_!_ : τ f]
                [x_!_ : τ f])
         e)
       (letrec ([x_!_ : τ f]
                [x_!_ : τ f]
                [x_!_ : τ f])
         e))
  (app ::=
       ()
       (e)
       (e e)
       (e e e)
       (e e e e)
       (o e e))
  (f ::= (lambda () e)
         (lambda ([x_!_ : τ]) e)
         (lambda ([x_!_ : τ] [x_!_ : τ]) e)
         (lambda ([x_!_ : τ] [x_!_ : τ] [x_!_ : τ]) e))
  ;; GTLC Types
  (ι ::= () Int Bool)
  (? ::= Dyn)
  (A ::= (-> τ) (τ -> τ) (τ τ -> τ) (τ τ τ -> τ))
  (τ ::= ι A ?)
  ;; Patterns that are necissary but unimportant
  (b ::= boolean)
  (i ::= integer)
  (o ::= + - * =)
  (x ::= variable-not-otherwise-mentioned)
  (Γ ::= · (x τ Γ)))

(define-lang-lookupo Estimate-GTLC (lookupo lookup Γ x τ))
(define-lang-extendo Estimate-GTLC (extendo extend Γ x τ))

(define-metafunction Estimate-GTLC
  tyop : o -> τ
  [(tyop +) (Int Int -> Int)]
  [(tyop -) (Int Int -> Int)]
  [(tyop *) (Int Int -> Int)]
  [(tyop =) (Int Int -> Bool)])


(define-judgment-form Estimate-GTLC
  #:mode (~≈ I I)
  #:contract (~≈ τ τ)
  [------------------------ "≈Refl"
            (~≈ τ τ)]

  [------------------------ "≈Right"
            (~≈ ? τ)]
  [------------------------ "≈Left"
            (~≈ τ ?)]
  [(~≈ τ_1 τ_2)
   ---------------------------------------- "≈Cong0"
   (~≈ (-> τ_1) (-> τ_2))]
  [(~≈ τ_1 τ_2)
   (~≈ τ_11 τ_21)
   ---------------------------------------- "≈Cong1"
   (~≈ (τ_11 -> τ_1) (τ_21 -> τ_2))]
  [(~≈ τ_1 τ_2)
   (~≈ τ_11 τ_21)
   (~≈ τ_12 τ_22)
   ---------------------------------------- "≈Cong2"
   (~≈ (τ_11 τ_12 -> τ_1) (τ_21 τ_22 -> τ_2))]
  [(~≈ τ_1 τ_2)
   (~≈ τ_11 τ_21)
   (~≈ τ_12 τ_22)
   (~≈ τ_13 τ_23)
   ---------------------------------------- "≈Cong3"
   (~≈ (τ_11 τ_12 τ_13 -> τ_1) (τ_21 τ_22 τ_23 -> τ_2))])

(module+ test
  (redex-check Estimate-GTLC #:satisfying (~≈ τ_1 τ_2)
               (judgment-holds (≈ τ_1 τ_2))
               #:attempts 20))

(define-judgment-form Estimate-GTLC
  #:mode (~≤ I I O)
  #:contract (~≤ τ τ τ)
  [------------------------ "≤Refl"
            (~≤ τ τ τ)]

  [------------------------ "≤Right"
            (~≤ ? τ τ)]
  [------------------------ "≤Left"
            (~≤ τ ? τ)]
  [(~≤ τ_1 τ_2 τ_3)
   ---------------------------------------- "≤Cong0"
   (~≤ (-> τ_1) (-> τ_2) (-> τ_3))]
  [(~≤ τ_1 τ_2 τ_3)
   (~≤ τ_11 τ_21 τ_31)
   ---------------------------------------- "≤Cong1"
   (~≤ (τ_11 -> τ_1) (τ_21 -> τ_2) (τ_31 -> τ_3))]
  [(~≤ τ_1 τ_2 τ_3)
   (~≤ τ_11 τ_21 τ_31)
   (~≤ τ_12 τ_22 τ_32)
   ---------------------------------------- "≤Cong2"
   (~≤ (τ_11 τ_12 -> τ_1) (τ_21 τ_22 -> τ_2) (τ_31 τ_32 -> τ_3))]
  [(~≤ τ_1 τ_2 τ_3)
   (~≤ τ_11 τ_21 τ_31)
   (~≤ τ_12 τ_22 τ_32)
   (~≤ τ_13 τ_23 τ_33)
   ---------------------------------------- "≤Cong3"
   (~≤ (τ_11 τ_12 τ_13 -> τ_1) (τ_21 τ_22 τ_23 -> τ_2) (τ_31 τ_32 τ_33 -> τ_3))])


(module+ test
  (redex-check Estimate-GTLC #:satisfying (~≤ τ_1 τ_2 τ_3)
               (and (judgment-holds (≈ τ_1 τ_2))
                    (judgment-holds (≈ τ_1 τ_3))
                    (judgment-holds (≈ τ_2 τ_3)))
               #:attempts 20))

(define-judgment-form Estimate-GTLC
  #:mode (wt I O)
  #:contract (wt e τ)
  [   (wt/env · e τ)
   --------------------- "Well Typed"
        (wt e τ)])

(define-judgment-form Estimate-GTLC
  #:mode (wt/env I I O)
  #:contract (wt/env Γ e τ)
  [(where (some τ) (lookup Γ x))
   ------------------------"~var"
   (wt/env Γ x τ)]

  [------------------------ "genInt"
                            (wt/env Γ i Int)]

  [------------------------ "genBool"
                            (wt/env Γ b Bool)]

  ;;lambda ----------------------------------------------------------------------
  [(wt/env Γ e τ_r)          
   ------------------------------------------------------- "genlam0"
   (wt/env Γ (lambda () e) (-> τ_r))]

  [(where Γ_1 (extend Γ x_1 τ_1))
   (wt/env Γ_1 e τ_r)
   ------------------------------------------------------- "genlam1"
   (wt/env Γ (lambda ([x_1 : τ_1]) e) (τ_1 -> τ_r))]

  [(where Γ_1 (extend Γ   x_1 τ_1))
   (where Γ_2 (extend Γ_1 x_2 τ_2))
   (wt/env Γ_2 e τ_r)
   ------------------------------------------------------- "genlam2"
   (wt/env Γ (lambda ([x_1 : τ_1] [x_2 : τ_2]) e) (τ_1 τ_2 -> τ_r))]

  [(where Γ_1 (extend Γ   x_1 τ_1))
   (where Γ_2 (extend Γ_1 x_2 τ_2))
   (where Γ_3 (extend Γ_2 x_3 τ_3))
   (wt/env Γ_3 e τ_r)
   ------------------------------------------------------- "genlam3"
   (wt/env Γ (lambda ([x_1 : τ_1] [x_2 : τ_2] [x_3 : τ_3]) e) (τ_1 τ_2 τ_3 -> τ_r))]

  ;;let ----------------------------------------------------------------------
  [(wt/env Γ e τ)
   ----------------------------------- "wt-let0"
   (wt/env Γ (let () e) τ)]
  
  [(where Γ_1 (extend Γ x_1 τ_1))
   (wt/env Γ e_1 τ_e1) (~≈ τ_e1 τ_1)
   (wt/env Γ_1 e τ)
   ----------------------------------- "wt-let1"
   (wt/env Γ (let ([x_1 : τ_1 e_1])
               e) τ)]
  
  [(where Γ_1 (extend Γ   x_1 τ_1))
   (where Γ_2 (extend Γ_1 x_2 τ_2))
   (wt/env Γ e_1 τ_e1) (~≈ τ_e1 τ_1)
   (wt/env Γ e_2 τ_e2) (~≈ τ_e2 τ_2)
   (wt/env Γ_2 e τ)
   ----------------------------------- "wt-let2"
   (wt/env Γ (let ([x_1 : τ_1 e_1]
                   [x_2 : τ_2 e_2])
               e) τ)]

  [(where Γ_1 (extend Γ   x_1 τ_1))
   (where Γ_2 (extend Γ_1 x_2 τ_2))
   (where Γ_3 (extend Γ_2 x_3 τ_3))
   (wt/env Γ    e_1 τ_e1) (~≈ τ_e1 τ_1)
   (wt/env Γ    e_2 τ_e2) (~≈ τ_e2 τ_2)
   (wt/env Γ    e_3 τ_e3) (~≈ τ_e3 τ_3)
   (wt/env Γ_3 e   τ)
   ----------------------------------- "wt-let3"
   (wt/env Γ (let ([x_1 : τ_1 e_1]
                   [x_2 : τ_2 e_2]
                   [x_3 : τ_3 e_3])
               e) τ)]
  
  ;;letrec ----------------------------------------------------------------------
  [(wt/env Γ e τ)
   ----------------------------------- "genletrec0"
   (wt/env Γ (letrec () e) τ)]
  
  [(where Γ_1 (extend Γ x S))
   (wt/env Γ_1 f S)
   (wt/env Γ_1 e τ)
   ----------------------------------- "genletrec1"
   (wt/env Γ (letrec ([x : S f]) e) τ)]

  [(where Γ_1 (extend Γ   x_1 τ_1))
   (where Γ_2 (extend Γ_1 x_2 τ_2))
   (wt/env Γ_2 f_1 τ_1)
   (wt/env Γ_2 f_2 τ_2)
   (wt/env Γ_2 e   τ)
   ----------------------------------- "genletrec2"
   (wt/env Γ (letrec ([x_1 : τ_1 f_1]
                      [x_2 : τ_2 f_2])
               e) τ)]

  [(where Γ_1 (extend Γ   x_1 τ_1))
   (where Γ_2 (extend Γ_1 x_2 τ_2))
   (where Γ_3 (extend Γ_2 x_3 τ_3))
   (wt/env Γ_3 f_1 τ_1)
   (wt/env Γ_3 f_2 τ_2)
   (wt/env Γ_3 f_3 τ_3)
   (wt/env Γ_3 e   τ)
   ----------------------------------- "genletrec3"
   (wt/env Γ (letrec ([x_1 : τ_1 f_1]
                      [x_2 : τ_2 f_2]
                      [x_3 : τ_3 f_3])
               e) τ)]

  ;; application ----------------------------------------------------------------------
  [------------------------ "gen()"
                            (wt/env Γ () ())]

  [(wt/env Γ e_0 (-> τ_r))
   -------------------------------------------------------------- "genapp0-fun"
   (wt/env Γ (e_0) τ_r)]

  [(wt/env Γ e_0 (τ_f1 -> τ_fr))
   (wt/env Γ e_1 τ_1)
   (~≈ τ_1 τ_f1)
   -------------------------------------------------------------- "genapp1-fun"
   (wt/env Γ (e_0 e_1) τ_fr)]

  [(wt/env Γ e_0 (τ_f1 τ_f2 -> τ_fr))
   (wt/env Γ e_1 τ_1) (~≈ τ_1 τ_f1)
   (wt/env Γ e_2 τ_2) (~≈ τ_2 τ_f2)
   -------------------------------------------------------------- "genapp2-fun"
   (wt/env Γ (e_0 e_1 e_2) τ_fr)]

  [(wt/env Γ e_0 (τ_f1 τ_f2 τ_f3 -> τ_fr))
   (wt/env Γ e_1 τ_1) (~≈ τ_1 τ_f1)
   (wt/env Γ e_2 τ_2) (~≈ τ_2 τ_f2)
   (wt/env Γ e_3 τ_3) (~≈ τ_3 τ_f3)
   -------------------------------------------------------------- "genapp3-fun"
   (wt/env Γ (e_0 e_1 e_2 e_3) τ_fr)]

  [(wt/env Γ e_0 Dyn)
   -------------------------------------------------------------- "genapp?0-fun"
   (wt/env Γ (e_0) Dyn)]

  [(wt/env Γ e_0 Dyn)
   (wt/env Γ e_1 τ_1)
   -------------------------------------------------------------- "genapp?1-fun"
   (wt/env Γ (e_0 e_1) Dyn)]

  [(wt/env Γ e_0 Dyn)
   (wt/env Γ e_1 τ_1) 
   (wt/env Γ e_2 τ_2)
   -------------------------------------------------------------- "genapp?2-fun"
   (wt/env Γ (e_0 e_1 e_2) Dyn)]

  [(wt/env Γ e_0 Dyn)
   (wt/env Γ e_1 τ_1) 
   (wt/env Γ e_2 τ_2) 
   (wt/env Γ e_3 τ_3) 
   -------------------------------------------------------------- "wtapp?3"
   (wt/env Γ (e_0 e_1 e_2 e_3) Dyn)]

  [(where (τ_f1 τ_f2 -> τ_fr) (tyop o))
   (wt/env Γ e_1 τ_1) (~≈ τ_1 τ_f1) 
   (wt/env Γ e_2 τ_2) (~≈ τ_2 τ_f2) 
   ------------------------ "wtOp"
   (wt/env Γ (o e_1 e_2) τ_fr)]

  [(wt/env Γ e_t τ_t) (~≈ τ_t Bool)
   (wt/env Γ e_c τ_c)
   (wt/env Γ e_a τ_a) (~≤ τ_c τ_a τ_meet)
   ------------------------ "wtIf"
   (wt/env Γ (if e_t e_c e_a) τ_meet)]

  [(wt/env Γ e τ_) (~≈ τ τ_)
   ------------------------ "wt:"
   (wt/env Γ (: e τ) τ)])

(module+ test
  (redex-check Estimate-GTLC #:satisfying (wt e ())
               (begin
                 (printf "\n() : ~a\n" (term e))
                 (judgment-holds (⊢_? () e ())))
               #:attempts 100
               #:attempt-size (lambda a 1))

  (redex-check Estimate-GTLC #:satisfying (wt e ι)
               (begin
                 (printf "\n~a : ~a\n" (term ι) (term e))
                 (judgment-holds (⊢_? () e ι)))
               #:attempts 100
               #:attempt-size (lambda a 1))

  (redex-check Estimate-GTLC #:satisfying (wt e any)
               (begin
                 (printf "\n~a : ~a\n" (term any) (term e))
                 (flush-output)
                 (judgment-holds (⊢_? () e any)))
               #:attempts 100
               #:attempt-size (lambda a 1))
  (redex-check Estimate-GTLC #:satisfying (wt e any)
               (begin
                 (printf "\n~a : ~a\n" (term any) (term e))
                 (flush-output)
                 (judgment-holds (⊢_? () e any)))
               #:attempts 100
               #:attempt-size (lambda a 2))
  (redex-check Estimate-GTLC #:satisfying (wt e any)
               (begin
                 (printf "\n~a : ~a\n" (term any) (term e))
                 (flush-output)
                 (judgment-holds (⊢_? () e any)))
               #:attempts 1000
               #:attempt-size (lambda a 3))
  (redex-check Estimate-GTLC #:satisfying (wt e any)
               (begin
                 (printf "\n~a : ~a\n" (term any) (term e))
                 (flush-output)
                 (judgment-holds (⊢_? () e any)))
               #:attempts 10000
               #:attempt-size (lambda a 4)))
