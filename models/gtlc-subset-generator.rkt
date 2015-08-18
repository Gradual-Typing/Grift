#lang racket/base

(require redex "gtlc.rkt")

(define-language GTLC/GEN
  (x   ::= variable-not-otherwise-mentioned)
  ;; Expression
  (e   ::= b i x f
       (letrec ([x : T f]) e)
       (letrec ([x : T f]
                [x : T f])
         e)
       (let ([x : T e]) e)
       (let ([x : T e]
             [x : T e])
         e)
       ()
       (e)
       (e e)
       (e e e)
       (e e e e)
       (if e e e)
       (o e e)
       (: e T))
  (o ::= + - * =)
  (f ::= (lambda () e)
         (lambda ([x : T]) e)
         (lambda ([x : T] [x : T]) e)
         (lambda ([x : T] [x : T] [x : T]) e))
  (b   ::= boolean)
  (i   ::= integer)
  ;; Labels
  (l       ::= string)
  ;; Optional Labels
  (p q     ::= ε l)
  ;; GTLC Types
  (B       ::= () Int Bool)
  (?       ::= Dyn)
  (A       ::= (-> T) (T -> T) (T T -> T) (T T T -> T))
  (R S T U ::= B A ?)
  ;; Labeled Types
  (P Q     ::= p)
  (Γ ::= () (Γ x T)))

(define-metafunction GTLC/GEN
  tyop/gen : o -> T
  [(tyop/gen +) (Int Int -> Int)]
  [(tyop/gen -) (Int Int -> Int)]
  [(tyop/gen *) (Int Int -> Int)]
  [(tyop/gen =) (Int Int -> Bool)])

(define-judgment-form GTLC/GEN
  #:mode (≈/gen I I)
  #:contract (≈/gen T T)
  [------------------------ "≈Refl"
            (≈/gen T T)]

  [------------------------ "≈Right"
            (≈/gen ? T)]
  [------------------------ "≈Left"
            (≈/gen T ?)]
  [(≈/gen T_1 T_2)
   ---------------------------------------- "≈Cong0"
   (≈/gen (-> T_1) (-> T_2))]
  [(≈/gen T_1 T_2)
   (≈/gen S_11 S_21)
   ---------------------------------------- "≈Cong1"
   (≈/gen (S_11 -> T_1) (S_21 -> T_2))]
  [(≈/gen T_1 T_2)
   (≈/gen S_11 S_21)
   (≈/gen S_12 S_22)
   ---------------------------------------- "≈Cong2"
   (≈/gen (S_11 S_12 -> T_1) (S_21 S_22 -> T_2))]
  [(≈/gen T_1 T_2)
   (≈/gen S_11 S_21)
   (≈/gen S_12 S_22)
   (≈/gen S_13 S_23)
   ---------------------------------------- "≈Cong3"
   (≈/gen (S_11 S_12 S_13 -> T_1) (S_21 S_22 S_23 -> T_2))])

(module+ test
  (redex-check GTLC/GEN #:satisfying (≈/gen T_1 T_2)
               (judgment-holds (≈ T_1 T_2))
               #:attempts 20))

(define-judgment-form GTLC/GEN
  #:mode (≤/gen I I O)
  #:contract (≤/gen T T T)
  [------------------------ "≤Refl"
            (≤/gen T T T)]

  [------------------------ "≤Right"
            (≤/gen ? T T)]
  [------------------------ "≤Left"
            (≤/gen T ? T)]
  [(≤/gen T_1 T_2 T_3)
   ---------------------------------------- "≤Cong0"
   (≤/gen (-> T_1) (-> T_2) (-> T_3))]
  [(≤/gen T_1 T_2 T_3)
   (≤/gen S_11 S_21 S_31)
   ---------------------------------------- "≤Cong1"
   (≤/gen (S_11 -> T_1) (S_21 -> T_2) (S_31 -> T_3))]
  [(≤/gen T_1 T_2 T_3)
   (≤/gen S_11 S_21 S_31)
   (≤/gen S_12 S_22 S_32)
   ---------------------------------------- "≤Cong2"
   (≤/gen (S_11 S_12 -> T_1) (S_21 S_22 -> T_2) (S_31 S_32 -> T_3))]
  [(≤/gen T_1 T_2 T_3)
   (≤/gen S_11 S_21 S_31)
   (≤/gen S_12 S_22 S_32)
   (≤/gen S_13 S_23 S_33)
   ---------------------------------------- "≤Cong3"
   (≤/gen (S_11 S_12 S_13 -> T_1) (S_21 S_22 S_23 -> T_2) (S_31 S_32 S_33 -> T_3))])


(module+ test
  (redex-check GTLC/GEN #:satisfying (≤/gen T_1 T_2 T_3)
               (and (judgment-holds (≈ T_1 T_2))
                    (judgment-holds (≈ T_1 T_3))
                    (judgment-holds (≈ T_2 T_3)))
               #:attempts 20))


(define-judgment-form GTLC/GEN
  #:mode (extendo I I I O)
  #:contract (extendo Γ x T Γ)
  [----------------------------- "Γ Cons"
    (extendo Γ x T (Γ x T))])

(define-judgment-form GTLC/GEN
  #:mode (lookupo I I O)
  #:contract (lookupo Γ x T)
  [----------------------------- "Γ head"
   (lookupo (Γ x T) x T)]
  [(where #f (same? x_1 x_2))
   (lookupo Γ x_2 T)
   ----------------------------- "Γ tail"
   (lookupo (Γ x_1 _) x_2 T)])

(module+ test
  (redex-check GTLC/GEN #:satisfying (extendo Γ x T Γ_new)
               (judgment-holds (lookupo Γ_new x T))
               #:attempts 20))


(define-metafunction GTLC/GEN
  same? : any any -> boolean
  [(same? any_1 any_1) #t]
  [(same? _ _) #f])

(define-judgment-form GTLC/GEN
  #:mode (⊢/gen I I O)
  #:contract (⊢/gen Γ e T)
  [     (lookupo Γ x T)
   ------------------------ "var"
        (⊢/gen Γ x T)]

  [------------------------ "genInt"
        (⊢/gen Γ i Int)]

  [------------------------ "genBool"
        (⊢/gen Γ b Bool)]
  
  [(⊢/gen Γ e T_r)          
   ------------------------------------------------------- "genlam0"
   (⊢/gen Γ (lambda () e) (-> T_r))]

  [(extendo Γ x_1 T_1 Γ_1)
   (⊢/gen Γ_1 e T_r)
   ------------------------------------------------------- "genlam1"
   (⊢/gen Γ (lambda ([x_1 : T_1]) e) (T_1 -> T_r))]

  [(where #f (same? x_1 x_2))
   (extendo Γ   x_1 T_1 Γ_1)
   (extendo Γ_1 x_2 T_2 Γ_2)
   (⊢/gen Γ_2 e T_r)
   ------------------------------------------------------- "genlam2"
   (⊢/gen Γ (lambda ([x_1 : T_1] [x_2 : T_2]) e) (T_1 T_2 -> T_r))]

  [(where #f (same? x_1 x_2))
   (where #f (same? x_1 x_3))
   (where #f (same? x_2 x_3))
   (extendo Γ   x_1 T_1 Γ_1)
   (extendo Γ_1 x_2 T_2 Γ_2)
   (extendo Γ_2 x_3 T_3 Γ_3)
   (⊢/gen Γ_3 e T_r)
   ------------------------------------------------------- "genlam3"
   (⊢/gen Γ (lambda ([x_1 : T_1] [x_2 : T_2] [x_3 : T_3]) e) (T_1 T_2 T_3 -> T_r))]

  [(extendo Γ x S Γ_1) (⊢/gen Γ_1 f S) (⊢/gen Γ_1 e T)
   ----------------------------------- "genletrec1"
   (⊢/gen Γ (letrec ([x : S f]) e) T)]
  [(where #f (same? x_1 x_2))
   (extendo Γ   x_1 S_1 Γ_1)
   (extendo Γ_1 x_2 S_2 Γ_2)
   (⊢/gen Γ_2 f_1 S_1)
   (⊢/gen Γ_2 f_2 S_2)
   (⊢/gen Γ_2 e T)
   ----------------------------------- "genletrec2"
   (⊢/gen Γ (letrec ([x_1 : S_1 f_1]
                     [x_2 : S_2 f_2])
              e) T)]

  [(extendo Γ x_1 S_1 Γ_1)
   (⊢/gen Γ e_1 S_1)
   (⊢/gen Γ_1 e T)
   ----------------------------------- "genlet2"
   (⊢/gen Γ (let ([x_1 : S_1 e_1])
              e) T)]
  
  [(where #f (same? x_1 x_2))
   (extendo Γ   x_1 S_1 Γ_1)
   (extendo Γ_1 x_2 S_2 Γ_2)
   (⊢/gen Γ e_1 S_1)
   (⊢/gen Γ e_2 S_2)
   (⊢/gen Γ_2 e T)
   ----------------------------------- "genlet1"
   (⊢/gen Γ (let ([x_1 : S_1 e_1]
                  [x_2 : S_2 e_2])
              e) T)]

  [------------------------ "gen()"
      (⊢/gen Γ () ())]
  [(⊢/gen Γ e_0 (-> T_r))
   -------------------------------------------------------------- "genapp0-fun"
   (⊢/gen Γ (e_0) T_r)]
  [(⊢/gen Γ e_0 (S_1 -> T_r)) (⊢/gen Γ e_1 T_1) (≈/gen T_1 S_1)
   -------------------------------------------------------------- "genapp1-fun"
   (⊢/gen Γ (e_0 e_1) T_r)]
  [(⊢/gen Γ e_0 (S_1 S_2 -> T_r))
   (⊢/gen Γ e_1 T_1) (≈/gen T_1 S_1) (⊢/gen Γ e_2 T_2) (≈/gen T_2 S_2)
   -------------------------------------------------------------- "genapp2-fun"
   (⊢/gen Γ (e_0 e_1 e_2) T_r)]
  [(⊢/gen Γ e_0 (S_1 S_2 S_3 -> T_r))
   (⊢/gen Γ e_1 T_1) (≈/gen T_1 S_1) (⊢/gen Γ e_2 T_2) (≈/gen T_2 S_2) (⊢/gen Γ e_3 T_3) (≈/gen T_3 S_3)
   -------------------------------------------------------------- "genapp3-fun"
   (⊢/gen Γ (e_0 e_1 e_2 e_3) T_r)]

  [(⊢/gen Γ e_0 Dyn)
   -------------------------------------------------------------- "genapp?0-fun"
   (⊢/gen Γ (e_0) Dyn)]
  [(⊢/gen Γ e_0 Dyn)
   (⊢/gen Γ e_1 T_1)
   -------------------------------------------------------------- "genapp?1-fun"
   (⊢/gen Γ (e_0 e_1) Dyn)]
  [(⊢/gen Γ e_0 Dyn)
   (⊢/gen Γ e_1 T_1) 
   (⊢/gen Γ e_2 T_2)
   -------------------------------------------------------------- "genapp?2-fun"
   (⊢/gen Γ (e_0 e_1 e_2) Dyn)]
  [(⊢/gen Γ e_0 Dyn)
   (⊢/gen Γ e_1 T_1) 
   (⊢/gen Γ e_2 T_2) 
   (⊢/gen Γ e_3 T_3) 
   -------------------------------------------------------------- "genapp?3-fun"
   (⊢/gen Γ (e_0 e_1 e_2 e_3) Dyn)]

  [(where (T_1 T_2 -> T_r) (tyop/gen o))
   (⊢/gen Γ e_1 T_1)
   (⊢/gen Γ e_2 T_2)
   ------------------------ "genapp-op"
   (⊢/gen Γ (o e_1 e_2) T_r)]
  [(⊢/gen Γ e_t T_t)
   (≈/gen T_t Bool)
   (⊢/gen Γ e_c T_c)
   (⊢/gen Γ e_a T_a)
   (≈/gen T_c T_a)
   (≤/gen T_c T_a T_meet)
   ------------------------ "gENIF"
   (⊢/gen Γ (if e_t e_c e_a) T_meet)])

(module+ test
  (redex-check GTLC/GEN #:satisfying (⊢/gen () e T)
               (judgment-holds (⊢ () e T))
               #:attempts 10000))
