#|
Author:      Deyaaeldeen Almahallawi (dalmahal@indiana.edu)
Description: Facilitates a user-friendly interface for Grift
             configurations. It is used mainly by the benchmarking
             scripts.
|#
#lang racket
(require racket/runtime-path)

(provide configs config->name name-sep name-end)

(define configs
  (make-hash
   ;; Index Cast-Mechinism
   ;; Ref-Mechinism
   ;; Cast-Specialization
   ;; Hybrid/Pure-Cast Runtime
   '((1 |Type-Based Casts| Proxied Unspecialized Eager UniformClosure)
     (2 Coercions Proxied Unspecialized Eager UniformClosure)
     (3 Hyper-Coercions Proxied Unspecialized Eager UniformClosure)
     (4 |Type-Based Casts| Monotonic Unspecialized Eager UniformClosure)
     (5 Coercions Monotonic Unspecialized Eager UniformClosure)
     (6 Hyper-Coercions Monotonic Unspecialized Eager UniformClosure)
     (7 |Type-Based Casts| Proxied Specialized Eager UniformClosure)
     (8 Coercions Proxied Specialized Eager UniformClosure)
     (9 Hyper-Coercions Proxied Specialized Eager UniformClosure) 
     (10 |Type-Based Casts| Monotonic Specialized Eager UniformClosure)
     (11 Coercions Monotonic Specialized Eager UniformClosure)
     (12 Hyper-Coercions Monotonic Specialized Eager UniformClosure)
     (13 Coercions Proxied Unspecialized Lazy UniformClosure)
     (14 Hyper-Coercions Proxied Unspecialized Lazy UniformClosure)
     (15 Coercions Monotonic Unspecialized Lazy UniformClosure)
     (16 Hyper-Coercions Monotonic Unspecialized Lazy UniformClosure)
     (17 Coercions Proxied Specialized Lazy UniformClosure)
     (18 Hyper-Coercions Proxied Specialized Lazy UniformClosure) 
     (19 Coercions Monotonic Specialized Lazy UniformClosure)
     (20 Hyper-Coercions Monotonic Specialized Lazy UniformClosure)
     (21 |Type-Based Casts| Proxied Unspecialized Eager NoUniformClosure)
     (22 Coercions Proxied Unspecialized Eager NoUniformClosure)
     (23 Hyper-Coercions Proxied Unspecialized Eager NoUniformClosure)
     (24 |Type-Based Casts| Monotonic Unspecialized Eager NoUniformClosure)
     (25 Coercions Monotonic Unspecialized Eager NoUniformClosure)
     (26 Hyper-Coercions Monotonic Unspecialized Eager NoUniformClosure)
     (27 |Type-Based Casts| Proxied Specialized Eager NoUniformClosure)
     (28 Coercions Proxied Specialized Eager NoUniformClosure)
     (29 Hyper-Coercions Proxied Specialized Eager NoUniformClosure) 
     (30 |Type-Based Casts| Monotonic Specialized Eager NoUniformClosure)
     (31 Coercions Monotonic Specialized Eager NoUniformClosure)
     (32 Hyper-Coercions Monotonic Specialized Eager NoUniformClosure)
     (33 Coercions Proxied Unspecialized Lazy NoUniformClosure)
     (34 Hyper-Coercions Proxied Unspecialized Lazy NoUniformClosure)
     (35 Coercions Monotonic Unspecialized Lazy NoUniformClosure)
     (36 Hyper-Coercions Monotonic Unspecialized Lazy NoUniformClosure)
     (37 Coercions Proxied Specialized Lazy NoUniformClosure)
     (38 Hyper-Coercions Proxied Specialized Lazy NoUniformClosure) 
     (39 Coercions Monotonic Specialized Lazy NoUniformClosure)
     (40 Hyper-Coercions Monotonic Specialized Lazy NoUniformClosure))))

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

  (define name-sep (make-parameter " "))
  (define name-end (make-parameter ""))

  ;; Defines ordering for elements of the configurations
  ;; names to be displayed
  (define (name-element<? e1 e2)
    (define classes
      '((Specialized Unspecialized)
        (Eager Lazy)
        (Proxied Monotonic)
        (Hyper-Coercions Coercions |Type-Based Casts|)
        (UniformClosure NoUniformClosure)))
    (define (find-class-rank e)
      (let loop ([cs classes])
        (if (set-member? (car cs) e)
            0
            (+ (loop (cdr cs)) 1))))
    (< (find-class-rank e1) (find-class-rank e2)))

  ;; Given a set of configuration options returns the stylized string
  ;; representation of that name.
  (define (set->name s [sep (name-sep)] [end (name-end)])
    (list->name (set->list s) sep end))

  (define (list->name l [sep (name-sep)] [end (name-end)])
    (define sls (sort l name-element<?))
    (define ss (map symbol->string sls)) 
    (string-join ss sep #:after-last end))
  
  (define (config->name c [sep (name-sep)] [end (name-end)])
    (list->name (hash-ref configs c) sep end))
    

(module+ main
  (define (default-main)
    (void))
  
  (define main-fn (make-parameter default-main))

  (command-line
   #:once-each
   [("--name-sep") sep
    "Set the seperator for name generation"
    (name-sep sep)]
   [("--name-end") end
    "Set the ending for name generation"
    (name-end end)]
   #:once-any
   [("--generate-configs-file" "-g")
    "Generate the configurations file"
    (write-configs!)]
   [("--update-configs" "-u")
    "Read configuration from the configurations file"
    (read-configs!)]
   [("--all" "-a")
    "Generate configuration strings for all configurations supported by Grift"
    (main-fn
     (lambda ()
      (display
       (string-join
        (for/list ([i (in-range 1 (+ 1 (hash-count configs)))])
          (set->name (list->set (hash-ref configs i))))
        ","))))]
   [("--names" "-n")
    "Generate configuration names from indicies"
    (main-fn
     (case-lambda
       ;; The singleton case does not work in the general case because
       ;; subtracting the set representing the configuration from itself yields
       ;; the empty set.
       [(x) (display (list->name (hash-ref configs (string->number x)) "_"))]
       [rest
        (define ns (map string->number rest))
        (define cs (map (lambda (n) (hash-ref configs n)) ns))
        (define ss (map list->set cs))
        (define common (apply set-intersect ss))
        (define ds (map (lambda (s) (set-subtract s common)) ss))
        (define names (map set->name ds))
        (display (string-join names ","))]))]
   [("--common")
    "Generate shared features of a set of configurations"
    (main-fn 
     (lambda rest
       (define ns (map string->number rest))
       (define cs (map (lambda (n) (hash-ref configs n)) ns))
       (define ss (map list->set cs))
       (define common (apply set-intersect ss))
       (define name (set->name common))
       (display name)))]
   [("--indices" "-i")
    "Generate configuration indices"
    (display
     (string-join
      (map number->string
           (range 1 (+ 1 (hash-count configs)))) " "))]
   [("--compare" "-c") c1 c2
    "compare and contrast two configurations"
    (define n1 (string->number c1))
    (define n2 (string->number c2))
    (cond
      [(and n1 n2)
       (define cs configs)
       (define s1 (list->set (hash-ref cs n1)))
       (define s2 (list->set (hash-ref cs n2)))
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
       (define-values (c1 c2)
         (let ([c1 (f s1 s2)] [c2 (f s2 s1)])
           (cond
             [(and (equal? "Eager Type-Based Casts" c1)
                   (equal? "Lazy Coercions" c2))
              (values "Type-Based Casts" "Coercions")]
             [(and (equal? "Eager Type-Based Casts" c2)
                   (equal? "Lazy Coercions" c1))
              (values "Coercions" "Type-Based Casts")]
             [else (values c1 c2)])))
       (define intersection
         (string-join
          (map symbol->string
               (set->list (set-intersect s1 s2))) "_")) 
       (printf "~a,~a,~a" c1 c2 intersection)]
      [else
       (error 'config_str: "could not parse ~a and ~a as numbers" c1 c2)])]
   #:args all
   (apply (main-fn) all)))
