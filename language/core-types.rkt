#lang racket
(provide (all-defined-out))

(struct Dyn ())
(struct Bool ())
(struct Int ())
(struct Function (from to))

(define (consistent? t g types)
  (or (Dyn? t) (Dyn? g)
      (and (Int? t) (Int? g))
      (and (Bool? t) (Bool? g))
      (and (Function? t) (Function? g)
           (let ((t-from (Function-from t))
                 (g-from (Function-from g))
                 (t-to (Function-to t))
                 (g-to (Function-to t)))
             (let ((t-arrity (vector-length t-from))
                   (g-arrity (vector-length g-from)))
               (and (= t-arrity g-arrity)
                    (let loop ((i 0))
                      (or (and (= i t-arrity) (consistent? t-to g-to)) 
                          (and (consistent? (vector-ref t-from i)
                                            (vector-ref g-from i))
                               (loop (add1 i)))))))))))

(define (meet-types t g err)
  (cond
    [(Dyn? t) g]
    [(Dyn? g) t]
    [(and (Int? t) (Int? g)) (Int)]
    [(and (Bool? t) (Bool? g)) (Bool)]
    [(and (Function? t) (Function? g))
     (let ((t-from (Function-from t))
           (g-from (Function-from g))
           (t-to (Function-to t))
           (g-to (Function-to t)))
       (let ((arrity (vector-length t-from)))
         (if (= arrity (vector-length g-from))
             (let ((m-from (make-vector arrity)))
               (do [(i 0 (add1 i))]
                   [(= i arrity) (Function m-from (meet-types t-to g-to))]
                 (vector-set! m-from i (meet-types (vector-ref t-from i)
                                                   (vector-ref t-from i)))))
             (err "Function type with incorrect arrity"))))]
    [else (err "Types are not consistent")]))

