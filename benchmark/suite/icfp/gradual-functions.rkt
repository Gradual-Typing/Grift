#lang racket

(require
 "helpers.rkt")

(initialize-dirs! "gradual-functions.rkt")

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

;; Iterations = the number of times the action in question is repeated
;; in order to make it large enough to measure acurately
(define iterations 10000)

;; Runs = the number of times the test is repeated in order to understand
;; how much volitility is coming from the system it is being run o
(define runs 30)

;; casts is a list of the number of casts to test in the benchmark
(define casts (build-list 11 (lambda (i) (* 2 i))))

;; The file is hard coded for each compiler configuration because
;; it makes formatting the each new data file easier.

(define decimals 4)


(define (make-timing-loop letrec-bnds
                          let-bnds
                          timed-action
                          untimed-action)
  `(letrec ,letrec-bnds
     (let ([iters : Int (read-int)]
           ,@let-bnds)
       (letrec ([run-test
                 : (Int -> ())
                 (lambda ([i : Int])
                   ,(timed-action 'i))])
         (begin
           (timer-start)
           (repeat (i 0 iters) (run-test i))
           (timer-stop)
           (timer-report)
           ,untimed-action)))))

(define timing-loop-example
  (make-timing-loop '(FUNC-BND ...)
                     '(DATA-BND ...)
                     (lambda (i) 'TIMED-CODE)
                     'UNTIME-CODE))

(define timing-loop-test
  (make-timing-loop
   '()
   '([acc : (GRef Int) (gbox 0)])
   (lambda (i) `(gbox-set! acc (+ ,i (gunbox acc))))
   '(gunbox acc)))

(define timing-loop-results
  (let ([src-file (write-source "timing-loop" timing-loop-test)]
        [spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : \\d+\n$"])
    `(("timing-loop-functional-twosomes"
       ,@(compile&run/iteration-time
          #:base-name     "timing-loop-functional-twosome"
          #:src-file      src-file
          #:runs          runs
          #:iterations    iterations
          #:cast-repr     'Twosomes
          #:function-repr 'Functional
          #:output-regexp spec))
      ("timing-loop-hybrid-coercions"
       ,@(compile&run/iteration-time
          #:base-name     "timing-loop-hybrid-coercions"
          #:src-file      src-file
          #:runs          runs
          #:iterations    iterations
          #:cast-repr     'Coercions
          #:function-repr 'Hybrid
          #:output-regexp spec)))))

(for ([r (in-list timing-loop-results)])
  (match-let ([(list name mean sdev) r])
    (printf "~a, ~a, ~a\n" name mean sdev)))


(define (make-cast-timing-loop-record name t1 t2 use-t1)
  (define (fresh-var x) (gensym 'x))
  (match-define `(,(and t1-args (app fresh-var vars)) ... -> ,t1-ret) t1)
  (define lam-bnds (map (lambda (x t) `[,x : ,t]) vars t1-args))
  `(,name
    ,t1 ,t2
    ,(make-timing-loop
      `([id  : ,t1 (lambda ,lam-bnds : ,t1-ret ,(car vars))])
      `([acc : (GRef ,t1) (gbox id)])
      (lambda (i) `(gbox-set! acc (: (: (gunbox acc) ,t2) ,t1)))
      (use-t1 '(gunbox acc)))))


(define function-cast-tests
  `(,(make-cast-timing-loop-record
      "cast-1.0"
      '(Int -> Int)
      '(Dyn -> Dyn)
      (lambda (v-t1) `(,v-t1 42)))
    ,(make-cast-timing-loop-record
      "cast-1.1"
      '((-> Int) -> (-> Int))
      '(Dyn -> Dyn)
      (lambda (v-t1) `((,v-t1 (lambda () 42)))))
    ,(make-cast-timing-loop-record
      "cast-1.2"
      '(Int Int -> Int)
      '(Dyn Dyn -> Dyn)
      (lambda (v-t1) `(,v-t1 42 777)))
    ,(make-cast-timing-loop-record
      "cast-1.3"
      '((Int -> Int) -> (Int -> Int))
      '(Dyn -> Dyn)
      (lambda (v-t1) `((,v-t1 (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-2.0"
      '((Int -> Int) -> (Int -> Int))
      '((Dyn -> Int) -> (Int -> Int))
      (lambda (v-t1) `((,v-t1 (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-2.1"
      '((Int -> Int) -> (Int -> Int))
      '((Dyn -> Dyn) -> (Int -> Int))
      (lambda (v-t1) `((,v-t1 (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-2.3"
      '((Int -> Int) -> (Int -> Int))
      '((Dyn -> Dyn) -> (Dyn -> Dyn))
      (lambda (v-t1) `((,v-t1 (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-2.4"
      '((Int -> Int) -> (Int -> Int))
      '((Dyn -> Dyn) -> (Dyn -> Dyn))
      (lambda (v-t1) `((,v-t1 (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-3.0"
      '(((-> Int) -> (-> Int)) -> ((-> Int) -> (-> Int)))
      '(((-> Dyn) -> (-> Int)) -> ((-> Int) -> (-> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda () 42)))))
    ,(make-cast-timing-loop-record
      "cast-3.1"
      '(((-> Int) -> (-> Int)) -> ((-> Int) -> (-> Int)))
      '(((-> Dyn) -> (-> Dyn)) -> ((-> Int) -> (-> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda () 42)))))
    ,(make-cast-timing-loop-record
      "cast-3.2"
      '(((-> Int) -> (-> Int)) -> ((-> Int) -> (-> Int)))
      '(((-> Dyn) -> (-> Dyn)) -> ((-> Dyn) -> (-> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda () 42)))))
    ,(make-cast-timing-loop-record
      "cast-3.3"
      '(((-> Int) -> (-> Int)) -> ((-> Int) -> (-> Int)))
      '(((-> Dyn) -> (-> Dyn)) -> ((-> Dyn) -> (-> Dyn)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda () 42)))))
    ,(make-cast-timing-loop-record
      "cast-4.0"
      '(((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      '(((Dyn -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-4.1"
      '(((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      '(((Dyn -> Dyn) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-4.2"
      '(((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      '(((Dyn -> Dyn) -> (Dyn -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-4.3"
      '(((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      '(((Dyn -> Dyn) -> (Dyn -> Dyn)) -> ((Int -> Int) -> (Int -> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-4.4"
      '(((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      '(((Dyn -> Dyn) -> (Dyn -> Dyn)) -> ((Dyn -> Int) -> (Int -> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-4.5"
      '(((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      '(((Dyn -> Dyn) -> (Dyn -> Dyn)) -> ((Dyn -> Dyn) -> (Int -> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-4.6"
      '(((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      '(((Dyn -> Dyn) -> (Dyn -> Dyn)) -> ((Dyn -> Dyn) -> (Int -> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-4.7"
      '(((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      '(((Dyn -> Dyn) -> (Dyn -> Dyn)) -> ((Dyn -> Dyn) -> (Dyn -> Int)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda (x) x)) 42)))
    ,(make-cast-timing-loop-record
      "cast-4.8"
      '(((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
      '(((Dyn -> Dyn) -> (Dyn -> Dyn)) -> ((Dyn -> Dyn) -> (Dyn -> Dyn)))
      (lambda (v-t1) `(((,v-t1 (lambda (x) x)) (lambda (x) x)) 42)))
    ))

(define function-cast-results
  (let ([spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : 42\n$"])
    (reverse
     (for/fold ([res '()])
               ([test (in-list function-cast-tests)])
       (match-let ([(list name t1 t2 prog) test])
         (define src-file (write-source name prog))
         (define name-twosomes-functional (string-append name "-twosomes-functional"))
         (define name-coercions-hybrid    (string-append name "-coercions-hybrid"))
         `((,name-twosomes-functional ,t1 ,t2 
            ,@(compile&run/iteration-time
               #:base-name     name-twosomes-functional
               #:src-file      src-file
               #:runs          runs
               #:iterations    iterations
               #:cast-repr     'Twosomes
               #:function-repr 'Functional
               #:output-regexp spec))
             (,name-coercions-hybrid ,t1 ,t2
              ,@(compile&run/iteration-time
                 #:base-name     name-coercions-hybrid
                 #:src-file      src-file
                 #:runs          runs
                 #:iterations    iterations
                 #:cast-repr     'Coercions
                 #:function-repr 'Hybrid
                 #:output-regexp spec))
           ,@res))))))

(for ([l function-cast-results])
  (match-let ([(list name t1 t2 mean sdev) l])
    (printf "~a, ~a, ~a\n" name mean sdev)))


(define (make-app-timing-loop-record name casts t1 t2 init use)
  (let-values ([(annotation invocation) 
                (if (or (symbol? casts) (even? casts))
                    (values `(,t1 -> ,t1) `(cast-loop ,casts id))
                    (values `(,t2 -> ,t2) `(: (cast-loop ,(sub1 casts) id) (,t2 -> ,t2))))])
    `(,name
      ,casts ,t1 ,t2
      ,(make-timing-loop
        `([cast-loop : (Int (,t1 -> ,t1) -> (,t1 -> ,t1))
                     (lambda ([n : Int] [g : (,t1 -> ,t1)])
                       (if (= n 0)
                           g 
                           (cast-loop (- n 2) (: g (Dyn -> Dyn)))))]
          [id        : (,t1 -> ,t1) (lambda ([x : ,t1]) x)])
        `([acc       : (GRef ,t1) (gbox ,init)]
          [id-casted : ,annotation ,invocation])
        (lambda (i) '(gbox-set! acc (id-casted (gunbox acc))))
        (use '(gunbox acc))))))

(define function-app-tests
  `(,@(for/list ([casts (in-range 0 10)])
        (make-app-timing-loop-record
         (format "app-int-dyn-~a" casts) casts 'Int 'Dyn 42
         (lambda (u) u)))
    ,@(for/list ([casts (in-range 0 10)])
        (make-app-timing-loop-record
         (format "app-fun-dyn-~a" casts) casts '(Int -> Int) 'Dyn
         '(lambda ([n : Int]) n)
         (lambda (u) `(,u 42))))
    ,@(for/list ([casts (in-range 0 10)])
        (make-app-timing-loop-record
         (format "app-fun-cast-~a" casts) casts '(Int -> Int) '(Dyn -> Dyn)
         '(lambda ([n : Int]) n)
         (lambda (u) `(,u 42))))))

(define function-app-results
  (let ([spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : 42\n$"])
    (reverse
     (for/fold ([res '()])
               ([test (in-list function-app-tests)])
       (match-let ([(list name casts t1 t2 prog) test])
         (define src-file (write-source name prog))
         (define name-twosomes-functional (string-append name "-twosomes-functional"))
         (define name-coercions-hybrid    (string-append name "-coercions-hybrid"))
         `((,name-twosomes-functional
            ,casts ,t1 ,t2 
            ,@(compile&run/iteration-time
               #:base-name     name-twosomes-functional
               #:src-file      src-file
               #:runs          runs
               #:iterations    iterations
               #:cast-repr     'Twosomes
               #:function-repr 'Functional
               #:output-regexp spec))
           (,name-coercions-hybrid
            ,casts ,t1 ,t2
            ,@(compile&run/iteration-time
               #:base-name     name-coercions-hybrid
               #:src-file      src-file
               #:runs          runs
               #:iterations    iterations
               #:cast-repr     'Coercions
               #:function-repr 'Hybrid
               #:output-regexp spec))
           ,@res))))))

(for ([l function-app-results])
  (match-let ([(list name casts t1 t2 mean sdev) l])
    (printf "~a, ~a, ~a\n" name mean sdev)))

