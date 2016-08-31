#lang racket

(require
 "helpers.rkt"
 "code-templates.rkt"
 syntax/location
 math)

;;(initialize-dirs! (file-name-from-path (quote-source-file)))

#|
This script runs a series of tests that are respnsible for
the following portion of the ICFP outline.

# Measure the performance of Gradual Functions
1. Experimental Conditions
   1. Measuring the speed of casts
      - show casting is always linear
   2. Measuring the speed of Applications
      - show applications can be constant time
   3. Measuring the worst case scenario
      - Depth of the cast
      - Normalization versus Application
   4. Include Formula 
      t_si = N_i * t_i + N_i * N_e * t_e 
      t_se = N_i * t_i +       N_e * t_e 
         where N_i = number of cast introductions
               t_i = time per cast (dependant on depth of types?)
               N_e = number of cast eliminations
               t_e = time per eliminations (dependent on depth of types?)
2. Results by Case Analysis on the Design Space
  1. SI Function Representation using twosomes
  -  SI Function Representation using coercions (don't have)
  -  SI Hybrid Representation using twosomes (don't have)
  -  SI Hybrid Representation using coercions (don't have)
  3. SE Hybrid Representation using Coercions

|#


#|--------------------------------------------------------------------
Configuaration variables
|#

;; Runs = the number of times the test is repeated in order to understand
;; how much volitility is coming from the system it is being run o
(define iters 1000)
(define runs  100)
(define decimals 2)
(define prec `(= ,decimals))


;; Casts can cast the same function over and over because the code
;; that is running will be the cast code.
;; Unfortunately this casts code is going to have unlikely cache performance.
;; This could be seen as best case cast behavior.
(define (make-function-types/init/use depth)
  (let loop ([depth depth])
    (cond
      [(<= depth 0)
       (values
        'Int
        'Dyn
        42
        (lambda (acc) acc))]
      [else
       (let-values ([(t1 t2 init use) (loop (sub1 depth))])
         (define t1^ `(,t1 -> ,t1))
         (define t2^ `(,t2 -> ,t2))
         (define init^ `(lambda ([x : ,t1]) x))
         (define use^ (lambda (acc) (use `(,acc ,init))))
         (values t1^ t2^ init^ use^))])))


(define (make-app-timing-loop-record name casts t1 t2 init use)
  (define id-type `(,t1 -> ,t1))
  (define id-type-inv `(,t2 -> ,t2))
  (list (format "fn-app-~a-~a" name casts)
        casts
        id-type id-type-inv
        (make-app-timing-loop casts t1 t2 init use)))

(define function-app-tests
  (append*
   (for/list ([casts (in-range 0 20)])
     (list
      (make-app-timing-loop-record
      "Int-Dyn"
      casts 'Int 'Dyn 42
      (lambda (u) u))
      (make-app-timing-loop-record
       "Fn0"
       casts '(Int -> Int) '(Dyn -> Dyn)
       '(lambda ([n : Int]) n)
       (lambda (u) `(,u 42)))
     (make-app-timing-loop-record
      "Fn1"
      casts
     '((Int -> Int) -> (Int -> Int))
     '((Dyn -> Dyn) -> (Dyn -> Dyn))
     '(lambda ([n : (Int -> Int)]) n)
     (lambda (u) `((,u (lambda (n) n)) 42)))))))

(define function-app-results
  (let ([spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : 42\n$"])
    (for/list ([test (in-list function-app-tests)])
      (match-let ([(list name casts t1 t2 prog) test])
        (define src-file (build-path src-dir (string-append "fn-app-for-stop-" name ".schml")))
        (write-source src-file prog)
        (define name-twosomes-functional (string-append name "-twosomes-functional"))
        (define name-coercions-hybrid    (string-append name "-coercions-hybrid"))
        (match-define (list t-run-time* t-iter-time*)
          (compile&run/iteration-time
           #:base-name     name-twosomes-functional
           #:src-file      src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Type-Based
           #:function-repr 'Functional
           #:output-regexp spec
           #:memory-limit  (* 4096 30000)
           #:mean-of-runs? #f))
        (match-define (list c-run-time* c-iter-time*)
          (compile&run/iteration-time
           #:base-name     name-coercions-hybrid
           #:src-file      src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Coercions
           #:function-repr 'Hybrid
           #:output-regexp spec
           #:memory-limit  (* 4096 30000)
           #:mean-of-runs?  #f))
        (list
         name casts t1 t2
         (mean-of-runs t-run-time* t-iter-time*)
         (list t-run-time* t-iter-time*)
         (mean-of-runs c-run-time* c-iter-time*)
         (list c-run-time* c-iter-time*))))))

(call-with-output-file
  "fn-app-for-stop-results-coercions=15.txt"
  #:exists 'replace
  (lambda (coercion-p)
    (display
     "# Casts twosomes-run twosomes-iter coercions-run coercions-iter\n"
     coercion-p)
    (for ([l function-app-results])
      (match-let ([(list name casts t1 t2
                         (list tmean tsdev) (list truns titers)
                         (list cmean csdev) (list cruns citers)) l])
        (match t1
          [`((,t1 -> t2) -> (,t3 -> ,t4))
           (for ([trun truns] [titer titers] [crun  cruns] [citer citers])
             (fprintf coercion-p "~a ~a ~a ~a ~a\n"
                     casts trun titer crun citer))])))))

(system "python fn-app-by-casts-for-type-based.py")
(system "python fn-app-by-casts-for-stop.py")

