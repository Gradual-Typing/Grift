#|
Author:      Deyaaeldeen Almahallawi (dalmahal@indiana.edu)
Description: Facilitates a user-friendly interface for Schml
             configurations. It is used mainly by the benchmarking
             scripts.
|#

#lang racket
(require racket/runtime-path)
;; Why are we writing this data structure to
;; a file. 
(define-runtime-path config-file "configs.dat")
;; Read the previous cofiguration state
(define (read-configs)
  (call-with-input-file config-file
    (lambda (in) (read in))))
;; Write some conguration state
(define (write-configs)
  (call-with-output-file
      config-file
      #:exists 'replace
      (lambda (in)
        (write
         (make-hash
          ;; Index Cast-Mechinism Ref-Mechinism Cast-Specialization Hybrid/Pure-Cast Runtime
          '((1 |Type-Based Casts| Proxied Interpreted Strict)
            (2 Coercions Proxied Interpreted Strict)
            (3 Hyper-Coercions Proxied Interpreted Strict)
            (4 |Type-Based Casts| Monotonic Interpreted Strict)
            (5 Coercions Monotonic Interpreted Strict)
            (6 Hyper-Coercions Monotonic Interpreted Strict)
            (7 |Type-Based Casts| Proxied Specialized Strict)
            (8 Coercions Proxied Specialized Strict)
            (9 Hyper-Coercions Proxied Specialized Strict) 
            (10 |Type-Based Casts| Monotonic Specialized Strict)
            (11 Coercions Monotonic Specialized Strict)
            (12 Hyper-Coercions Monotonic Specialized Strict)
            (13 Coercions Proxied Interpreted Lazy)
            (14 Hyper-Coercions Proxied Interpreted Lazy)
            (15 Coercions Monotonic Interpreted Lazy)
            (16 Hyper-Coercions Monotonic Interpreted Lazy)
            (17 Coercions Proxied Specialized Lazy)
            (18 Hyper-Coercions Proxied Specialized Lazy) 
            (19 Coercions Monotonic Specialized Lazy)
            (20 Hyper-Coercions Monotonic Specialized Lazy)))
         in))))

(module+ main  
  (command-line
   #:once-any
   [("--generate-configs-file" "-g")
    "Generate the configurations file"
    (write-configs)]
   [("--all" "-a")
    "Generate configuration strings for all configurations supported by Schml"
    (let ([cs (read-configs)])
      (display
       (string-join
        (for/list ([i (in-range 1 (+ 1 (hash-count cs)))])
          (string-join (map symbol->string (hash-ref cs i))
                       ".")) ",")))]
   [("--indices" "-i")
    "Generate configuration indices"
    (let ([cs (read-configs)])
      (display
       (string-join
        (map number->string
             (range 1 (+ 1 (hash-count cs)))) " ")))]
   [("--contrast" "-c") c1 c2
    "compare two configurations by indice"
    (define n1 (string->number c1))
    (define n2 (string->number c2))
    (cond
      [(and n1 n2)
       (define cs (read-configs))
       (define s1 (list->set (hash-ref cs n1)))
       (define s2 (list->set (hash-ref cs n2)))
       (define (f s1 s2)
         (define s (set-subtract s1 s2))
         (cond
           [(= (set-count s) 1) (symbol->string (set-first s))]
           [(and (set-member? s '|Type-Based Casts|) (set-member? s 'Pure)) "Pure Type-Based Casts"]
           [(and (set-member? s 'Coercions) (set-member? s 'Hybrid)) "Hybrid Coercions"]
           [else
            (error (string-append
                    "you should compare configurations that "
                    "differ in exactly one parameter but the difference is: "
                    (number->string (set-count s))
                    " and they are: "
                    (string-join (map symbol->string (set->list s1)))
                    " and "
                    (string-join (map symbol->string (set->list s2)))))]))
       (printf "~a,~a,~a"
               (f s1 s2)
               (f s2 s1)
               (string-join
                (map symbol->string
                     (set->list (set-intersect s1 s2))) "_"))]
      [else
       (error 'config_str: "could not parse ~a and ~a as numbers" c1 c2)])]))
