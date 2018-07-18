#lang typed/racket/base

(provide
 ;; I couldn't find a way to provide the struct but not the constructor
 (struct-out MSet)
 mset
 mset->list
 list->mset
 mset-add!
 mset-remove!
 mset-member?
 mset-union!)

;; Typed Racket Doesn't come with types for mutable sets. This library
;; implements mutable sets in terms of mutable hashtables
(struct (T) MSet ([contents : (Mutable-HashTable T True)])
  #:transparent)

(: mset : (All (T) -> (MSet T)))
(define (mset)
  (MSet ((inst make-hasheq T True) '())))

(: mset->list (All (T) (MSet T) -> (Listof T)))
(define (mset->list st) (hash-keys (MSet-contents st)))

(: list->mset (All (T) (Listof T) -> (MSet T)))
(define (list->mset ls)
  (define contents : (Mutable-HashTable T True) (make-hasheq))
  (for ([k (in-list ls)]) (hash-set! contents k #t))
  (MSet contents))

(: mset-add! (All (T) (MSet T) T -> Void))
(define (mset-add! st e)
  (hash-set! (MSet-contents st) e #t))

(: mset-remove! (All (T) (MSet T) T -> Void))
(define (mset-remove! st e)
  (hash-remove! (MSet-contents st) e))

(: mset-member? (All (T) (MSet T) T -> Boolean))
(define (mset-member? s e)
  (hash-has-key? (MSet-contents s) e))

(: mset-union! (All (T) (MSet T) (MSet T) -> Void))
(define (mset-union! mst0 mst1)
  (define st0 (MSet-contents mst0))
  (define st1 (MSet-contents mst1))
  (for ([k (in-hash-keys st1)])
    (hash-set! st0 k #t)))
