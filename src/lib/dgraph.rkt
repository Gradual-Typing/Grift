#lang typed/racket/base/no-check

(require "dhash.rkt")
(provide
 ;; I couldn't figure out a way to provide the type but not the struct.
 (struct-out DGraph)
 make-dgraph
 dgraph?
 dgraph-add-vertex!
 dgraph-has-vertex?
 dgraph-add-edge!
 dgraph-has-edge?
 dgraph-remove-edge!
 dgraph-vertices
 in-dgraph-vertices
 in-dgraph-edges-from
 dgraph-edges-from
 dgraph-scc)

(module+ test
  (require rackunit))

(define-type (DG N) (DHash N (DHash N True)))

(struct (N) DGraph ([contents : (DG N)])
  #:transparent)

(: make-dgraph (All (N) (->* () ((Listof (Pairof N (Listof N)))) (DGraph N))))
(define (make-dgraph [als : (Listof (Pairof N (Listof N))) '()])
  (define contents : (DG N) (make-dhasheq))
  (for ([row als])
    (define v  (car row))
    (define e* (cdr row))
    (define e*-from-v (%add-vertex! contents v))
    (for ([w (in-list e*)])
      (%add-vertex! contents w)
      (dhash-set! e*-from-v w #t)))
  (DGraph contents))

;; add a vertice to the graph if it hasn't be done yet
(: %add-vertex! : (All (N) (DG N) N -> (DHash N True)))
(define (%add-vertex! h v)
  (dhash-ref! h v (λ () : (DHash N True) (make-dhasheq))))

(define dgraph? DGraph?)

(module+ test
  (define als '((v w) (w v x) (x) (z)))
  (define g1 : (DGraph Symbol) (make-dgraph als))
  (test-equal? "graph? true"  (dgraph? g1) #t)
  (test-equal? "graph? false" (dgraph? als) #f))

(: dgraph-has-vertex? : (All (N) (DGraph N) N -> Boolean))
(define (dgraph-has-vertex? g v)
  (dhash-has-key? (DGraph-contents g) v))

(module+ test
  (test-equal? "has-vertex? true" (dgraph-has-vertex? g1 'v) #t)
  (test-equal? "has-vertex? false" (dgraph-has-vertex? g1 'a) #f))

(: dgraph-has-edge? (All (A) (DGraph A) A A -> Boolean))
(define (dgraph-has-edge? g u v)
  (define e*-from-u? (dhash-ref (DGraph-contents g) u #f))
  (and e*-from-u? (dhash-ref e*-from-u? v #f)))

(module+ test
  (test-equal? "has-edge? true 1" (dgraph-has-edge? g1 'v 'w) #t)
  (test-equal? "has-edge? true 2" (dgraph-has-edge? g1 'w 'x) #t)
  (test-equal? "has-edge? false 1" (dgraph-has-edge? g1 'a 'x) #f)
  (test-equal? "has-edge? false 2" (dgraph-has-edge? g1 'x 'w) #f))

(: %add-edge! (All (A) (DG A) A A -> Void))
(define (%add-edge! h u v)
  (define e*-from-u (%add-vertex! h u))
  (define e*-from-v (%add-vertex! h v))
  (dhash-set! e*-from-u v #t))

(: dgraph-add-edge! (All (A) (DGraph A) A A -> Void))
(define (dgraph-add-edge! g u v)
  (%add-edge! (DGraph-contents g) u v))

(: dgraph-remove-edge! (All (A) (DGraph A) A A -> Void))
(define (dgraph-remove-edge! g u v)
  (define edges-from-u? : (Option (DHash A True))
    (dhash-ref (DGraph-contents g) u #f))
  (when edges-from-u?
    (dhash-remove! edges-from-u? v)))

(: dgraph-add-vertex! (All (A) (DGraph A) A -> Void))
(define (dgraph-add-vertex! g v)
  (%add-vertex! (DGraph-contents g) v)
  (void))

(: dgraph-vertices (All (A) (DGraph A) -> (Listof A)))
(define (dgraph-vertices g) (dhash-keys (DGraph-contents g)))

(: in-dgraph-vertices (All (A) (DGraph A) -> (Sequenceof A)))
(define (in-dgraph-vertices g)
  (in-dhash-keys (DGraph-contents g)))

(: in-dgraph-edges-from (All (A) (DGraph A) A -> (Sequenceof A)))
(define (in-dgraph-edges-from g a)
  (in-dhash-keys (dhash-ref (DGraph-contents g) a)))

(: dgraph-edges-from (All (A) (DGraph A) A -> (Listof A)))
(define (dgraph-edges-from g a)
  (dhash-keys (dhash-ref (DGraph-contents g) a)))

(struct node
  ([index : Natural]
   [low-link : Natural]
   [on-stack? : Boolean])
  #:mutable
  #:transparent)

;; Tarjan's algorithm for strongly connected components
(: dgraph-scc : (All (N) (DGraph N) -> (Listof (Listof N))))
(define (dgraph-scc G)
  (define nodes : (Mutable-HashTable N node) (make-hasheq))
  (define index : Natural 0) ;; Preorder index
  (define stack : (Listof (Pairof N node)) '())
  (define sccs  : (Listof (Listof N)) '())
  
  (for ([v (in-dgraph-vertices G)])
    (unless (hash-has-key? nodes v)
      (let strong-connect : node ([v : N v])
        (define node-v (node index index #t))
        (hash-set! nodes v node-v)
        ;; Push v on stack
        (set! stack (cons (cons v node-v) stack))
        (set! index (add1 index))
        (for ([w (in-dgraph-edges-from G v)])
          (define node-w (hash-ref nodes w #f))
          (cond
            [(not node-w)
             (define node-w (strong-connect w))
             (define w.ll (node-low-link node-w))
             (define v.ll (node-low-link node-v))
             (set-node-low-link! node-v (min w.ll v.ll))]
            [(node-on-stack? node-w)
             ;; Successor w is in stack S and hence in the current SCC
             ;; Note: The next line may look odd - but is correct.  It
             ;; says w.index not w.lowlink; that is deliberate and
             ;; from the original paper
             (define v.ll (node-low-link node-v))
             (define w.i  (node-index node-w))
             (set-node-low-link! node-v (min v.ll w.i))]
            ;; If w is not on stack, then (v, w) is a cross-edge in
            ;; the DFS tree and must be ignored
            [else (void)]))
        
        (when (= (node-low-link node-v) (node-index node-v))
          (define c : (Listof N)
            (let loop ([c : (Listof N) '()])
              ;; Pop w off of stack
              (define p (car stack))
              (define w (car p))
              (define w-node (cdr p))
              (set! stack (cdr stack))
              (set-node-on-stack?! w-node #f)
              (let ([c (cons w c)])
                (if (eq? v w) c (loop c)))))
          (set! sccs (cons c sccs)))
        node-v)))
  sccs)
