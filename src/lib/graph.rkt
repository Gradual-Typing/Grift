#lang typed/racket/base

(provide
 ;; I couldn't figure out a way to provide the type but not the struct.
 (struct-out Graph)
 make-graph
 graph?
 add-vertex!
 has-vertex?
 add-edge!
 has-edge?
 remove-edge!
 get-vertices
 in-vertices
 in-edges-from
 edges-from
 scc)

(module+ test
  (require
   typed/rackunit))

(define-type (G N)
  (Mutable-HashTable N (Mutable-HashTable N True)))
;; Graph is a directed eq? graph with explicit
;; support for backtracking edges.
;; to vertices, and walking the graph.
;; - `to` = inverted graph represented as edges to each vertice
;; - `from` = regular graph represented as edges from vertices
;; invarients:
;; - (v, e) in `to` <-> (v, e) in `from`
(struct (N) Graph ([contents : (G N)])
  #:transparent)

(: make-graph : (All (N) (Listof (Pairof N (Listof N))) -> (Graph N)))
(define (make-graph als)
  (define contents : (G N) (make-hasheq))
  (for ([row als])
    (define v  (car row))
    (define e* (cdr row))
    (define e*-from-v (%add-vertex! contents v))
    (for ([w (in-list e*)])
      (%add-vertex! contents w)
      (hash-set! e*-from-v w #t)))
  (Graph contents))

;; add a vertice to the graph if it hasn't be done yet
(: %add-vertex! : (All (N) (G N) N -> (Mutable-HashTable N True)))
(define (%add-vertex! h v)
  (hash-ref! h v (λ () : (Mutable-HashTable N True)(make-hasheq))))

(define graph? Graph?)

(module+ test
  (define als '((v w) (w v x) (x) (z)))
  (define g1 : (Graph Symbol) (make-graph als))
  (test-equal? "graph? true"  (graph? g1) #t)
  (test-equal? "graph? false" (graph? als) #f))

(: has-vertex? : (All (N) (Graph N) N -> Boolean))
(define (has-vertex? g v)
  (hash-has-key? (Graph-contents g) v))

#;(: in-edges-of-vertex : (All (N) (Graph N) N -> (Sequenceof N (G N))))
#;
(define/match (in-edges-of-vertex g v)
  [((Graph from _) v) (in-hash (hash-ref from v))])

#;(: in-edges-to-vertex : (All (N) (Graph N) N -> (Sequenceof N (G N))))
#;
(define/match (in-edges-to-vertex g v)
  [((Graph _ to) v) (in-hash to)])

#;(: in-vertices : (All (N) (Graph N) N -> (Sequenceof N)))
#;
(define/match (in-vertices G)
  [((Graph from _)) (in-hash-keys from)])

#;(: in-edges : (All (N) (Graph N) -> (Sequenceof N (G N))))
#;
(define/match (in-edges G)
  [((Graph from _)) (in-hash from)])

(module+ test
  (test-equal? "has-vertex? true" (has-vertex? g1 'v) #t)
  (test-equal? "has-vertex? false" (has-vertex? g1 'a) #f))

(: has-edge? (All (A) (Graph A) A A -> Boolean))
(define (has-edge? g u v)
  (define e*-from-u? (hash-ref (Graph-contents g) u #f))
  (and e*-from-u? (hash-ref e*-from-u? v #f)))

(module+ test
  (test-equal? "has-edge? true 1" (has-edge? g1 'v 'w) #t)
  (test-equal? "has-edge? true 2" (has-edge? g1 'w 'x) #t)
  (test-equal? "has-edge? false 1" (has-edge? g1 'a 'x) #f)
  (test-equal? "has-edge? false 2" (has-edge? g1 'x 'w) #f))

(: %add-edge! (All (A) (G A) A A -> Void))
(define (%add-edge! h u v)
  (define e*-from-u (%add-vertex! h u))
  (define e*-from-v (%add-vertex! h v))
  (hash-set! e*-from-u v #t))

(: add-edge! (All (A) (Graph A) A A -> Void))
(define (add-edge! g u v)
  (%add-edge! (Graph-contents g) u v))

(: remove-edge! (All (A) (Graph A) A A -> Void))
(define (remove-edge! g u v)
  (define edges-from-u? (hash-ref (Graph-contents g) u #f))
  (when edges-from-u?
    (hash-remove! edges-from-u? v)))

(: add-vertex! (All (A) (Graph A) A -> Void))
(define (add-vertex! g v)
  (%add-vertex! (Graph-contents g) v)
  (void))

(: get-vertices (All (A) (Graph A) -> (Listof A)))
(define (get-vertices g) (hash-keys (Graph-contents g)))

(: in-vertices (All (A) (Graph A) -> (Sequenceof A)))
(define (in-vertices g) (in-mutable-hash-keys (Graph-contents g)))

(: in-edges-from (All (A) (Graph A) A -> (Sequenceof A)))
(define (in-edges-from g a)
  (in-mutable-hash-keys (hash-ref (Graph-contents g) a)))

(: edges-from (All (A) (Graph A) A -> (Listof A)))
(define (edges-from g a)
  (hash-keys (hash-ref (Graph-contents g) a)))

(struct node
  ([index : Natural]
   [low-link : Natural]
   [on-stack? : Boolean])
  ;; I really want mutable nongenerative
  #:mutable
  #:transparent)

;; Tarjan's algorithm for strongly connected components
(: scc : (All (N) (Graph N) -> (Listof (Listof N))))
(define (scc G)
  (define nodes : (Mutable-HashTable N node) (make-hasheq))
  (define index : Natural 0) ;; Preorder index
  (define stack : (Listof (Pairof N node)) '())
  (define sccs  : (Listof (Listof N)) '())
  
  (for ([v (in-vertices G)])
    (unless (hash-has-key? nodes v)
      (let strong-connect : node ([v : N v])
        (define node-v (node index index #t))
        (hash-set! nodes v node-v)
        ;; Push v on stack
        (set! stack (cons (cons v node-v) stack))
        (set! index (add1 index))
        (for ([w (in-edges-from G v)])
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
