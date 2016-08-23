#lang typed/racket/base

(provide make-gvector
         gvector-set!
         gvector-grow!
         gvector->list
         gvector-ref
         gvector->vector
         gvector-length
         (except-out (struct-out GVectorof) set-GVectorof-init! set-GVectorof-data!))

;; Growable vectors
(struct (A) GVectorof ([init : A] [data : (Vectorof A)])
  #:mutable
  #:transparent)

(: make-gvector (All (a) (Natural a -> (GVectorof a))))
(define (make-gvector len init)
  (GVectorof init (make-vector len init)))

(: gvector-set-without-grow! (All (a) ((GVectorof a) Natural a -> Void)))
(define (gvector-set-without-grow! gv i v)
  (unless (< i (gvector-length gv))
    (error 'gvector-set! "index out of bounds"))
  (vector-set! (GVectorof-data gv) i v))

(: gvector-set! (All (a) ((GVectorof a) Natural a -> Void)))
(define (gvector-set! gv i v)
  (cond
    [(< i (vector-length (GVectorof-data gv)))
     (gvector-set-without-grow! gv i v)]
    [else
     (gvector-grow! gv (add1 i))
     (gvector-set-without-grow! gv i v)]))

(: gvector-grow! (All (a) ((GVectorof a) Natural -> Void)))
(define (gvector-grow! gv min)
  (define-values (init data)
    (values (GVectorof-init gv) (GVectorof-data gv)))
  (define old-len (vector-length data))
  (define new-len
    (do : Natural ([l : Natural old-len (* l 2)])
        ((< min l) l)))
  (set-GVectorof-data! gv (make-vector new-len init))
  ;; Todo test this with vector-copy!
  (for ([i : Natural (in-range 0 old-len)])
    (gvector-set-without-grow! gv i (vector-ref data i))))

(: gvector-length (All (a) (GVectorof a) -> Index))
(define (gvector-length x)
  (vector-length (GVectorof-data x)))

(: gvector->list : All (A) (GVectorof A) -> (Listof A))
(define (gvector->list gv)
  (vector->list (GVectorof-data gv)))

(: gvector->vector : All (A) (GVectorof A) -> (Vectorof A))
(define gvector->vector GVectorof-data)


(: gvector-ref (All (A) (GVectorof A) Natural -> A))
(define (gvector-ref gv i)
  (unless (< i (gvector-length gv))
    (error 'gvector-ref "index ~a out of bound for ~a" i gv))
  (vector-ref (GVectorof-data gv) i))


