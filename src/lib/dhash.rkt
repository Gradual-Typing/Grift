#lang typed/racket/base

(require racket/match)
(provide (all-defined-out))


(struct (K V) DHash ([content : (Mutable-HashTable K V)] [seq : (Listof K)])
  #:mutable
  #:transparent)

(: make-dhasheq (All (K V) (->* () ((Listof (Pair K V))) (DHash K V))))
(define (make-dhasheq [als : (Listof (Pair K V)) '()])
  (define ht : (Mutable-HashTable K V) (make-hasheq))
  (define dht : (DHash K V) (DHash ht '()))
  (for ([p als])
    (match p
      [(cons k v) (dhash-set! dht k v)]
      [_ (raise-argument-error 'make-dhasheq "alist?" als)]))
  dht)

(: dhash-set! (All (K V) (DHash K V) K V -> Void))
(define (dhash-set! dht k v)
  (match-define (DHash c s) dht)
  (define before-count (hash-count c))
  (hash-set! c k v)
  (unless (= before-count (hash-count c))
    (set-DHash-seq! dht (cons k s))))

(: missing-key (All (K V) K -> (-> V)))
(define ((missing-key k))
  (error 'dhash-ref "key, ~a, not found" k))

(: dhash-ref
   (All (K V)
        (case->
         [(DHash K V) K -> V]
         [(DHash K V) K False -> (Option V)]
         [(DHash K V) K (-> V) -> V])))
(define (dhash-ref dht k [default 'dhash-ref/no-default-passed])
  (cond
    [(eq? default 'dhash-ref/no-default-passed)
     (hash-ref (DHash-content dht) k (missing-key k))]
    [else (hash-ref (DHash-content dht) k default)]))

(: dhash-ref! (All (K V) (DHash K V) K (-> V) -> V))
(define (dhash-ref! dht k v)
  (match-define (DHash c s) dht)
  (define (th-v) : V
    (set-DHash-seq! dht (cons k s))
    (v))
  (hash-ref! c k th-v))

(: dhash-has-key? (All (K V) (DHash K V) K -> Boolean))
(define (dhash-has-key? dht k)
  (hash-has-key? (DHash-content dht) k))

(: dhash-remove! (All (K V) (DHash K V) K -> Void))
(define (dhash-remove! dht k)
  (hash-remove! (DHash-content dht) k))

(: dhash-keys (All (K V) (DHash K V) -> (Listof K)))
(define (dhash-keys dht)
  (match-define (DHash c s) dht)
  (: has? : K -> Boolean)
  (define (has? k) (hash-has-key? c k))
  (filter has? s))

(: in-dhash-keys (All (K V) (DHash K V) -> (Sequenceof K)))
(define (in-dhash-keys dht) (in-list (dhash-keys dht)))
