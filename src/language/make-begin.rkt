#lang typed/racket/base

(require "forms.rkt"
         "data1.rkt"
         "data2.rkt"
         "data3.rkt"
         "data4.rkt"
         "data5.rkt")

(provide make-begin)


;; Sometimes the order of the types is important but I have
;; not figured out a good system for figuring out what the
;; order should be except trial and error.
(: make-begin
   (case->
    (-> D3-Effect* No-Op D3-Effect)
    (-> D3-Effect* D3-Value D3-Value)
    (-> D3-Effect* D3-Tail  D3-Tail)
    (-> D3-Effect* D3-Pred  D3-Pred)
    #;(-> C7-Effect* C7-Value  C7-Value)
    #;(-> C7-Effect* No-Op C7-Effect)
#;  (-> D3-Effect* D3-Trivial D3-Value)
    (-> D2-Effect* D2-Value  D2-Value)
    (-> D2-Effect* D2-Pred D2-Pred)
    (-> D2-Effect* No-Op D2-Effect)
    (-> D1-Effect* D1-Value  D1-Value)
    (-> D1-Effect* No-Op D1-Effect)
    (-> D4-Effect* D4-Tail D4-Tail)
    (-> D4-Effect* No-Op D4-Effect)
    (-> D5-Effect* D5-Tail D5-Tail)))

;; make a begin language form but splice nested begins into the
;; newly made begin.
(define (make-begin eff* res)
  (let ([eff* (foldr splice-eff '() eff*)])
    (cond
      [(null? eff*) res]
      ;; In effect position a begin of one effect is the ;
      ;; same as the effect alone
      [(and (No-Op? res) (null? (cdr eff*))) (car eff*)]
      ;; If the result is a begin I assume that the begin
      ;; was made with make-begin.
      [(Begin? res)
       (Begin (append eff* (Begin-effects res)) (Begin-value res))]
      [else (Begin eff* res)])))

(: splice-eff
   (case->
    ;; Since D2 effect is a strict subset of D1-effect
    ;; It must come before D1 because the first type to
    ;; match is used
    (-> D5-Effect D5-Effect* D5-Effect*)
    (-> D4-Effect D4-Effect* D4-Effect*)
    (-> D3-Effect D3-Effect* D3-Effect*)
    #;(-> C7-Effect C7-Effect* C7-Effect*)
    (-> D2-Effect D2-Effect* D2-Effect*)
    (-> D1-Effect D1-Effect* D1-Effect*)))
(define (splice-eff eff rest)
  (cond
    [(No-Op? eff) rest]
    [(Begin? eff) (foldr splice-eff rest (Begin-effects eff))]
    [else (cons eff rest)]))
