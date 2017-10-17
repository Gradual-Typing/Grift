#|
Author:      Deyaaeldeen Almahallawi (dalmahal@indiana.edu)
Description: Facilitates a user-friendly interface for Schml configurations.
It is used mainly by the benchmarking scripts.
|#

#lang racket
(require racket/runtime-path)
(require racket/struct)

(define valid-cast-semantics (list->set '(|Type-Based Casts| Coercions Hyper-Coercions)))

(define valid-ref-semantics (list->set '(Proxied Monotonic)))

(define valid-cast-specs (list->set '(Specialized Unspecialized)))

(define valid-coercion-creation-semantics (list->set '(Strict Lazy)))

(define-struct config (cast-semantics ref-semantics cast-specs coercion-creation-semantics)
  #:transparent
  #:guard
  (lambda (cast-semantics ref-semantics cast-specs coercion-creation-semantics name)
    (define (check n s e)
      (unless (set-member? s e)
        (error (string->symbol (format "make-~a" name))
               (string-append
                "field #" n " must be one of "
                (string-join (map symbol->string (set->list s)))))))
    
    (check "1" valid-cast-semantics cast-semantics)
    (check "2" valid-ref-semantics ref-semantics)
    (check "3" valid-cast-specs cast-specs)
    (check "4" valid-coercion-creation-semantics coercion-creation-semantics)
    (values cast-semantics ref-semantics cast-specs coercion-creation-semantics)))

(provide (struct-out config) configs)
(define configs
  (make-hash
   `((1 . ,(make-config '|Type-Based Casts| 'Proxied 'Unspecialized 'Strict))
     (2 . ,(make-config 'Coercions 'Proxied 'Unspecialized 'Strict))
     (3 . ,(make-config 'Hyper-Coercions 'Proxied 'Unspecialized 'Strict))
     (4 . ,(make-config '|Type-Based Casts| 'Monotonic 'Unspecialized 'Strict))
     (5 . ,(make-config 'Coercions 'Monotonic 'Unspecialized 'Strict))
     (6 . ,(make-config 'Hyper-Coercions 'Monotonic 'Unspecialized 'Strict))
     (7 . ,(make-config '|Type-Based Casts| 'Proxied 'Specialized 'Strict))
     (8 . ,(make-config 'Coercions 'Proxied 'Specialized 'Strict))
     (9 . ,(make-config 'Hyper-Coercions 'Proxied 'Specialized 'Strict)) 
     (10 . ,(make-config '|Type-Based Casts| 'Monotonic 'Specialized 'Strict))
     (11 . ,(make-config 'Coercions 'Monotonic 'Specialized 'Strict))
     (12 . ,(make-config 'Hyper-Coercions 'Monotonic 'Specialized 'Strict))
     (13 . ,(make-config 'Coercions 'Proxied 'Unspecialized 'Lazy))
     (14 . ,(make-config 'Hyper-Coercions 'Proxied 'Unspecialized 'Lazy))
     (15 . ,(make-config 'Coercions 'Monotonic 'Unspecialized 'Lazy))
     (16 . ,(make-config 'Hyper-Coercions 'Monotonic 'Unspecialized 'Lazy))
     (17 . ,(make-config 'Coercions 'Proxied 'Specialized 'Lazy))
     (18 . ,(make-config 'Hyper-Coercions 'Proxied 'Specialized 'Lazy)) 
     (19 . ,(make-config 'Coercions 'Monotonic 'Specialized 'Lazy))
     (20 . ,(make-config 'Hyper-Coercions 'Monotonic 'Specialized 'Lazy)))))

;; Why are we writing this data structure to
;; a file. 

(define-runtime-path config-file "configs.dat")
;; Read the previous cofiguration state

(define (read-configs!)
  (set! configs (call-with-input-file config-file
                  (lambda (in) (read in)))))

;; Write some conguration state
(define (write-configs!)
  (call-with-output-file config-file #:exists 'replace
    (lambda (in)
      (write configs in))))

(module+ main
  (define (default-main)
    (void))
  
  (define main-fn (make-parameter default-main))
  
  (command-line
   #:once-any
   [("--generate-configs-file" "-g")
    "Generate the configurations file"
    (write-configs!)]
   [("--update-configs" "-u")
    "Read configuration from the configurations file"
    (read-configs!)]
   [("--all" "-a")
    "Generate configuration strings for all configurations supported by Schml"
    (display
     (string-join
      (for/list ([i (in-range 1 (+ 1 (hash-count configs)))])
        (string-join (map symbol->string (struct->list (hash-ref configs i)))
                     ".")) ","))]
   [("--indices-to-names")
    "Generate configuration names from indicies"
    (main-fn 
     (lambda rest
       (display
        (string-join
         (for/list ([i (map string->number rest)])
           (string-join (map symbol->string (struct->list (hash-ref configs i)))
                        ".")) ","))))]
   [("--indices" "-i")
    "Generate configuration indices"
    (display
     (string-join
      (map number->string
           (range 1 (+ 1 (hash-count configs)))) " "))]
   [("--contrast" "-c")
    c1 c2
    "compare two configurations by indice"
    (define n1 (string->number c1))
    (define n2 (string->number c2))
    (cond
      [(and n1 n2)
       (define cs configs)
       (define s1 (list->set (struct->list (hash-ref cs n1))))
       (define s2 (list->set (struct->list (hash-ref cs n2))))
       (define (f s1 s2)
         (define s (set-subtract s1 s2))
         (cond
           [(= (set-count s) 1) (symbol->string (set-first s))]
           [(and (set-member? s '|Type-Based Casts|) (set-member? s 'Strict))
            "Strict Type-Based Casts"]
           [(and (set-member? s 'Coercions) (set-member? s 'Lazy))
            "Lazy Coercions"]
           [else
            (string-join (map symbol->string (set->list s)) " ")]))
       (printf "~a,~a,~a"
               (f s1 s2)
               (f s2 s1)
               (string-join
                (map symbol->string
                     (set->list (set-intersect s1 s2))) "_"))]
      [else
       (error 'config_str: "could not parse ~a and ~a as numbers" c1 c2)])]
   #:args all
   (apply (main-fn) all)))
