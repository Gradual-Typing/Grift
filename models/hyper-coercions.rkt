#lang racket/base

(require redex/reduction-semantics "stlc.rkt")
(provide (all-defined-out))

(define-extended-language λhc STLC
  (e  ::= .... (coerce e c))
  (ℓ  ::= string)
  (τ  ::= T Dyn (τ ... -> τ))
  (p  ::= ε ℓ) 
  (i  ::= ε !) 
  (m  ::= ε (-> (c ...n -> c) (τ ...n -> τ)))
  (c  ::= (hc p τ m i) (hc⊥ p τ ℓ)))

(module+ test
  (test-equal (redex-match? λhc T (term Int)) #t)
  (test-equal (redex-match? λhc τ (term Int)) #t)
  (test-equal (term (make-coercion Int Int  "l")) (term (hc ε Int ε ε)))
  (test-equal (term (make-coercion Int Bool "l")) (term (hc⊥ ε Int "l")))
  (test-equal (term (make-coercion Dyn Int  "l")) (term (hc "l" Int ε ε)))
  (test-equal (term (make-coercion Int Dyn  "l")) (term (hc ε Int ε !)))
  (test-equal (term (make-coercion Dyn Dyn  "l")) (term (hc ε Dyn ε ε)))
  (test-equal (term (make-coercion Dyn (Int -> Bool) "l"))
              (term (hc "l"
                        (Int -> Bool)
                        (-> ((hc ε Int ε ε) -> (hc ε Bool ε ε))
                            (Int -> Bool))
                        ε)))
  (test-equal (term (make-coercion (Dyn -> Dyn) (Int -> Bool) "l"))
              (term (hc ε
                        (Dyn -> Dyn)
                        (-> ((hc ε Int ε !) -> (hc "l" Bool ε ε))
                            (Int -> Bool))
                        ε))))

(define-metafunction λhc
  make-coercion : τ τ ℓ -> c
  [(make-coercion B   B   ℓ) (hc ε B   ε ε)]
  [(make-coercion Dyn Dyn ℓ) (hc ε Dyn ε ε)]
  [(make-coercion Dyn B   ℓ) (hc ℓ B   ε ε)]
  [(make-coercion B   Dyn ℓ) (hc ε B   ε !)]
  [(make-coercion Dyn (τ_n ... -> τ_r) ℓ)
   (hc ℓ
       (τ_n ... -> τ_r)
       (-> ((make-coercion τ_n τ_n ℓ) ... -> (make-coercion τ_r τ_r ℓ))
           (τ_n ... -> τ_r))
       ε)]
  [(make-coercion (τ_n ... -> τ_r)  Dyn ℓ)
   (hc ε
       (τ_n ... -> τ_r)
       (-> ((make-coercion τ_n τ_n ℓ) ... -> (make-coercion τ_r τ_r ℓ))
           (τ_n ... -> τ_r))
       !)]
  [(make-coercion (τ_1 ...n -> τ_2) (τ_3 ...n -> τ_4) ℓ)
   (hc ε
       (τ_1 ... -> τ_2)
       (-> ((make-coercion τ_3 τ_1 ℓ) ... -> (make-coercion τ_2 τ_4 ℓ))
           (τ_3 ... -> τ_4))
       ε)]
  [(make-coercion τ_1 τ_2 ℓ) (hc⊥ ε τ_1 ℓ)])








