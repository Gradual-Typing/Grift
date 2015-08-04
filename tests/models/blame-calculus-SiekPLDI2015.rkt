#lang racket

(require redex)

(define-language λB
  (i     ::= Nat)
  (p     ::= string)
  (n     ::= Natural)
  (k     ::= n)
  (x     ::= variable-not-otherwise-mentioned)
  ;; Types
  (A B ::= i (A -> B) ★)
  ;; Gound types (why is ⋆ -> ⋆ here)
  ;; This seems to indicate that this is the lazy variant 
  ;; perhaps this should be translated as injectable types
  (G H     ::= i (★ -> ★))
  ;; Expression of the language
  (L M N   ::= k (op M ...) x (λ (x : A) N) (L M) (M : A => B p) (blame p))
  ;; Values
  (V W     ::= k (λ (x : A) N) (V : (A -> B) => (A -> B) p) (V : G => ★))
  (E       ::= hole (op V ... E M ...) (E M) (V E) (E : A => B p)))

(render-language λB #:nts '(A B G L M N V W E))


(define-judgment-form λB
  #:mode (∼ I I)
  #:contract (∼ A B)
  ;; Reflexive – This could be formulated as (∼ A A)
  [---------------- 
   (∼ i i)]
  ;; Congurence
  [(∼ A_1 B_1) (∼ A_2 B_2)
   ----------------
   (∼ (A_1 -> A_2) (B_1 -> B_2))]
  ;; Dynamic left and right
  [(∼ A ⋆)]
  [(∼ ⋆ B)])

(render-judgment-form ∼)

;; =α : are two expression alpha equivalent?
(define-metafunction λB
  =α : any any -> boolean
  [(=α any_1 any_2) ,(equal? (term (sd any_1))
                             (term (sd any_2)))])

(define (=α/racket t1 t2) (term (=α ,t1 ,t2)))

;; sd converts regular bindings to static distance
;; this allows us to just check if the types and graph are
;; the same
(define-extended-language λΔ λB
 (L M N ::= .... (K n) (λ A M))
 (Γ ::= (x ...)))

(define-metafunction λΔ
  sd : any -> any
  [(sd any) (sd/env any ())])

;; Helper function for converting to static distance representation
;; the anys here may allow me to reuse the function on languages that
;; are derived from the λB
(define-metafunction λΔ
  sd/env : any Γ -> any
  ;; Bound variables get a debruijn index
  [(sd/env x (x_1 ... x x_2 ...))
   (K ,(length (term (x_1 ...))))
   (side-condition (not (member (term x) (term (x_1 ...)))))]
  ;; Free variables are compared by name
  [(sd/env x _) x]
  ;; Lambdas must be of the correct type
  [(sd/env (λ (x : A) M) (x_1 ...))
   (λ A (sd/env M (x x_1 ...)))]
  [(sd/env (any ...) Γ) ((sd/env any Γ) ...)]
  ;[(sd/env any Γ) any]
  )

(module+ test
  (test-equal (term (=α (λ (x : ★) (x x))
                        (λ (y : ★) (y y))))
              #t)
  (test-equal (term (=α (λ (x : ★)
                          (λ (y : ★)
                            (y x)))
                        (λ (x : ★)
                          (λ (y : ★)
                            (x y)))))
              #f))



(module+ test (test-results))


