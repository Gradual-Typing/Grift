#lang racket

(require
 "helpers.rkt"
 "code-templates.rkt"
 syntax/location
 math)

(initialize-dirs! (file-name-from-path (quote-source-file)))

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

(define timing-loop-test
  (make-timing-loop
   #:timed-action  (lambda (i acc) `(+ ,i ,acc))
   #:acc-type 'Int
   #:acc-init 0
   #:use-acc-action (lambda (acc) acc)))


(define timing-loop-results
  (let ([src-file (write-source "timing-loop" timing-loop-test)]
        [spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : \\d+\n$"])
    (for/list ([iters (range 2000 3000 1000)]) 
      (list iters
            (compile&run/iteration-time
             #:base-name     "timing-loop-functional-twosome"
             #:src-file      src-file
             #:runs          runs
             #:iterations    iters
             #:cast-repr     'Twosomes
             #:function-repr 'Functional
             #:output-regexp spec)
            (compile&run/iteration-time
             #:base-name     "timing-loop-hybrid-coercions"
             #:src-file      src-file
             #:runs          runs
             #:iterations    iters
             #:cast-repr     'Coercions
             #:function-repr 'Hybrid
             #:output-regexp spec)))))

(with-output-to-file
  "timing-loop.tex"
  #:exists 'replace
  (lambda ()
    (display
     (string-append
      "\\begin{tabular}{| l | r | r |}\n"
      "  \\hline\n"
      "& Type-Based & Coercions \\\\\n"
      "  \\hline\n"
      "  Iterations & \\multicolumn{2}{c|}{Time($n s$)/Iteration}\\\\\n"
      "  \\hline\n"))
    (for ([result (in-list timing-loop-results)])
      (match-let ([(list iters (list tmean tsdev) (list cmean csdev)) result])
        (printf  "  ~a & ~a & ~a \\\\\n"
                 iters
                 (~r tmean #:precision prec)
                 (~r cmean #:precision prec))
        (display "  \\hline\n")))
    (display "\\end{tabular}\n")))

(define (make-function-types/init/use depth args)
  (let loop ([depth depth])
    (cond
      [(<= depth 0)
       (define t1 (append (make-list args 'Int) '(-> Int)))
       (define t2 (append (make-list args 'Dyn) '(-> Dyn)))
       (define-values (init use)
         (if (<= args 0)
             (values `(lambda () 42) (lambda (f) `(,f)))
             (let* ([args (cons 42 (make-list (sub1 args) -1))] 
                    [fmls (for/list ([a args]) (gensym 'x))]
                    [bnds (for/list ([x fmls]) `[,x : Int])])
               (values
                `(lambda ,bnds ,(car fmls)) (lambda (f) `(,f ,@args))))))
       (values t1 t2 init use)]
      [else
       (let-values ([(t1 t2 init use) (loop (sub1 depth))])
         (define t1^ `(,t1 -> ,t1))
         (define t2^ `(,t2 -> ,t2))
         (define use^ (lambda (f) (use `(,f ,init))))
         (define init^ `(lambda ([x : ,t1]) x))
         (values t1^ t2^ init^ use^))])))

(define (make-cast-timing-loop-record depth args)
  (define-values (t1 t2 init use)
    (make-function-types/init/use depth args))
  (list (format "fn-cast-~a-~a" depth args)
        depth args
        (sizeof-type* t1 t2) (sizeof-coercion-of-types t1 t2)
        t1 t2
        (make-timing-loop
         #:timed-action  (lambda (i acc) `(: (: acc ,t2) ,t1))
         #:letrec-bnds  `([f : ,t1 ,init])
         #:acc-type t1
         #:acc-init 'f
         #:use-acc-action use)))

(define function-cast-tests
  (for*/list ([depth (in-range 0 5)]
              [args  (in-range 1 2)]) ;; Just 1 for now
    (make-cast-timing-loop-record depth args)))

(define function-cast-results
  (let ([spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : 42\n$"])
    (for/list ([test (in-list function-cast-tests)])
      (match-let ([(list name depth args st sc t1 t2 prog) test])
        (define src-file (write-source name prog))
        (define name-twosomes-functional (string-append name "-twosomes-functional"))
        (define name-coercions-hybrid    (string-append name "-coercions-hybrid"))
        (match-define (list t-run-time* t-iter-time*)
          (compile&run/iteration-time
           #:base-name     name-twosomes-functional
           #:src-file      src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Twosomes
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
         name depth args st sc t1 t2
         (mean-of-runs t-run-time* t-iter-time*)
         (list t-run-time* t-iter-time*)
         (mean-of-runs c-run-time* c-iter-time*)
         (list c-run-time* c-iter-time*))))))

(with-output-to-file
  "partially-typed-general-statistics.txt"
  #:exists 'replace
  (lambda ()
    (define-values (titers)
      (for/fold ([i '()])
                ([l function-cast-results])
        (match-let ([(list name depth args st sc t1 t2
                           (list tmean tsdev) (list truns titers)
                           (list cmean csdev) c-points) l])
          (append titers i))))
    (printf "twosomes count: ~a\n" (length titers))
    (printf "fn cast twosomes total time(us): ~a\n" (sum titers))
    (printf "fn cast twosomes mean  time(us): ~a\n" (mean titers))
    (printf "fn cast twosomes sdev(us):       ~a\n" (stddev titers))))

(with-output-to-file
  "partially-typed-function-cast.tex"
  #:exists 'replace
  (lambda ()
    (display
     (string-append
      "\\begin{tabular}{| c | r | r |}\n"
      "  \\hline\n"
      "  & Type-Based & Coercions \\\\\n"
      "  \\hline\n"
      "  Type/Coercion Size & "
      "\\multicolumn{2}{c|}{Time($n s$)/Iteration} \\\\\n"
      "  \\hline\n"))
    (for ([l function-cast-results])
      (match-let ([(list name depth args st sc t1 t2
                         (list tmean tsdev) t-points
                         (list cmean csdev) c-points) l])
        (printf "  ~a / ~a & ~a & ~a \\\\\n"
                st sc
                (~r tmean #:precision prec)
                (~r cmean #:precision prec))
        (display "  \\hline\n")))
    (display "\\end{tabular}\n")))

(with-output-to-file
  "partially-typed-function-cast-twosomes.txt"
  #:exists 'replace
  (lambda ()
    (display
     (string-append
      "# partially typed function casts twosomes\n"
      "# sizeof-types sizeof-coercions time(us)/iteration\n"))
    (for ([l function-cast-results])
      (match-let ([(list name _ _ st sc _ _ _ (list _ iter-times) _ _) l])
        (for ([time iter-times])
          (printf "~a ~a ~a\n" st sc time))))))
(with-output-to-file
  "partially-typed-function-cast-coercions.txt"
  #:exists 'replace
  (lambda ()
    (display
     (string-append
      "# partially typed function casts coercions\n"
      "# sizeof-types sizeof-coercions time(us)/iteration\n"))
    (for ([l function-cast-results])
      (match-let ([(list name _ _ st sc _ _ _ _ _ (list _ c-iter-times)) l])
        (for ([time c-iter-times])
          (printf "~a ~a ~a\n" st sc time))))))

(define (make-app-timing-loop-record name casts t1 t2 init use)
  (define id-type `(,t1 -> ,t1))
  (define id-type-inv `(,t2 -> ,t2))
  (list (format "fn-app-~a-~a" name casts)
        casts
        (sizeof-type* id-type id-type-inv)
        (sizeof-coercion-of-types id-type id-type-inv)
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
     (lambda (u) `((,u (lambda (n) n)) 42)))
     (make-app-timing-loop-record
     "Fn2"
     casts
     '(((Int -> Int) -> (Int -> Int)) -> ((Int -> Int) -> (Int -> Int)))
     '(((Dyn -> Dyn) -> (Dyn -> Dyn)) -> ((Dyn -> Dyn) -> (Dyn -> Dyn)))
     '(lambda ([n : ((Int -> Int) -> (Int -> Int))]) n)
     (lambda (u) `(((,u (lambda (n) n)) (lambda (n) n)) 42)))))))

(define function-app-results
  (let ([spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : 42\n$"])
    (for/list ([test (in-list function-app-tests)])
      (match-let ([(list name casts st sc t1 t2 prog) test])
        (define src-file (write-source name prog))
        (define name-twosomes-functional (string-append name "-twosomes-functional"))
        (define name-coercions-hybrid    (string-append name "-coercions-hybrid"))
        (match-define (list t-run-time* t-iter-time*)
          (compile&run/iteration-time
           #:base-name     name-twosomes-functional
           #:src-file      src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Twosomes
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
         name casts st sc t1 t2
         (mean-of-runs t-run-time* t-iter-time*)
         (list t-run-time* t-iter-time*)
         (mean-of-runs c-run-time* c-iter-time*)
         (list c-run-time* c-iter-time*))))))

(define (latex-fn-app-data res name)
  (with-output-to-file
    (string-append "partially-typed-function-app-" name ".tex")
    #:exists 'replace
    (lambda ()
      (display
       (string-append
        "\\begin{tabular}{| c | c | r | r |}\n"
        "  \\hline\n"
        "  \\multicolumn{2}{|c|}{} & Type-Based & Coercions \\\\\n"
        "  \\hline\n"
        "  Casts & Type/Coercion Size & "
        "     \\multicolumn{2}{c|}{Time($ns$)/Interation} \\\\\n"
        "  \\hline\n"))
      (for ([l res])
        (match-let ([(list name casts st sc t1 t2
                           (list tmean tsdev) _
                           (list cmean csdev) _) l])
          (printf "  ~a & ~a / ~a & ~a & ~a \\\\\n"
                  casts
                  st sc
                  (~r tmean #:precision prec)
                  (~r cmean #:precision prec))
          (display "  \\hline\n")))
      (display "\\end{tabular}\n"))))

(define ((app-res-casts<? n) r)
  (match-let ([(list name casts _ ...) r])
    (< casts n)))

(define ((app-res-casts>=? n) r)
  (not ((app-res-casts<? n) r)))

(latex-fn-app-data (filter (app-res-casts<? 10)  function-app-results) "0-9")
(latex-fn-app-data (filter (app-res-casts>=? 10) function-app-results) "10-19")


(call-with-output-file
  #;"partially-typed-function-app-results-coercions=63.txt"
  "partially-typed-function-app-results-coercions=15.txt"
  #:exists 'replace
  (lambda (coercion-p)
    (call-with-output-file
      "partially-typed-function-app-results-casts=1.txt"
      #:exists 'replace
      (lambda (cast-p)
        (display
         "# Casts types coercions twosomes-run twosomes-iter coercions-run coercions-iter\n"
         coercion-p)
                (display
         "# Casts types coercions twosomes-run twosomes-iter coercions-run coercions-iter\n"
         cast-p)
        (for ([l function-app-results])
          (match-let ([(list name casts st sc t1 t2
                             (list tmean tsdev) (list truns titers)
                             (list cmean csdev) (list cruns citers)) l])
            (when (= casts 1)
              (for ([trun truns] [titer titers] [crun  cruns] [citer citers])
                (fprintf cast-p "~a ~a ~a ~a ~a ~a ~a\n"
                         casts st sc trun titer crun citer)))
            (when (= sc 15) #;(= sc 63)
              (for ([trun truns] [titer titers] [crun  cruns] [citer citers])
                (fprintf coercion-p "~a ~a ~a ~a ~a ~a ~a\n"
                         casts st sc trun titer crun citer)))))))))

(define (make-reference-cast-timing-loop-record depth)
  (define t1 (symbolic-repeat depth 'GRef 'Int))
  (define t2 (symbolic-repeat depth 'GRef 'Dyn))
  (list (format "ref-cast-~a" depth)
        (sizeof-type* t1 t2)
        (sizeof-coercion-of-types t1 t2)
        t1 t2
        (make-reference-cast-timing-loop t1 t2 depth)))

(define reference-cast-tests
  (for/list ([depth (in-range 0 6)])
    (make-reference-cast-timing-loop-record depth)))

(define reference-cast-results
  (let ([spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : 42\n$"])
    (for/list ([test (in-list reference-cast-tests)])
      (match-let ([(list name st sc t1 t2 prog) test])
        (define src-file (write-source name prog))
        (define name-twosomes   (string-append name "-twosomes"))
        (define name-coercions  (string-append name "-coercions"))
        (match-define (list t-run-time* t-iter-time*)
          (compile&run/iteration-time
           #:base-name     name-twosomes
           #:src-file      src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Twosomes
           #:function-repr 'Functional
           #:output-regexp spec
           #:memory-limit  (* 4096 30000)
           #:mean-of-runs? #f))
        (match-define (list c-run-time* c-iter-time*)
          (compile&run/iteration-time
           #:base-name     name-coercions
           #:src-file      src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Coercions
           #:function-repr 'Hybrid
           #:output-regexp spec
           #:memory-limit  (* 4096 30000)
           #:mean-of-runs?  #f))
        (list
         name st sc t1 t2
         (mean-of-runs t-run-time* t-iter-time*)
         (list t-run-time* t-iter-time*)
         (mean-of-runs c-run-time* c-iter-time*)
         (list c-run-time* c-iter-time*))))))

(call-with-output-file
  "partially-typed-reference-cast.tex"
  #:exists 'replace
  (lambda (tex-out)
    (call-with-output-file
      "partially-typed-reference-cast.txt"
      #:exists 'replace
      (lambda (data-out)
        (display
     (string-append
      "\\begin{tabular}{| c | r | r |}\n"
      "  \\hline\n"
      "  & Type-Based & Coercions \\\\\n"
      "  \\hline\n"
      "  Type/Coercion Size & "
      "     \\multicolumn{2}{c|}{Time($ns$)/Interation} \\\\\n"
      "  \\hline\n")
     tex-out)
        (display "#type coercions twosome-runs twosome-iters coercions-runs coercion-iters\n"
                 data-out)
        (for ([l reference-cast-results])
          (match-let ([(list name st sc t1 t2
                             (list tmean tsdev) (list truns titers)
                             (list cmean csdev) (list cruns citers)) l])
            (fprintf tex-out
                     "  ~a / ~a & ~a & ~a\\\\\n"
                     st sc
                     (real->decimal-string tmean decimals)
                     (real->decimal-string cmean decimals))
            (display "  \\hline\n" tex-out)
            (for ([tr truns] [ti titers] [cr cruns] [ci citers])
              (fprintf data-out
                       "~a ~a ~a ~a ~a ~a\n"
                       st sc tr ti cr ci))))
        (display "\\end{tabular}\n" tex-out)))))

#|
(define (make-reference-read-timing-loop-record casts depth)
  ;; cadr here subtracts on symbolic repeat
  (define t1 (symbolic-repeat depth 'GRef 'Int))
  (define t2 (symbolic-repeat depth 'GRef 'Dyn))
  (define ref (symbolic-repeat depth 'gbox 42))
  (define use (lambda (v) (cadr (symbolic-repeat depth 'gunbox v))))
  (define-values (acc-type cl-invocation acc-init use-undyned)
    (if (even? casts)
        (values (cadr t1)
                `(cast-loop ,casts ,ref)
                (cadr (symbolic-repeat depth 'gbox -1)) 
                (lambda (use) use))
        (values (cadr t2)
                `(: (cast-loop ,(sub1 casts) ,ref) ,t2)
                (cadr (symbolic-repeat depth 'gbox '(: -1 Dyn))) 
                (lambda (use) (lambda (v) `(: ,(use v) Int))))))
  (list (format "ref-read-~a-~a" casts depth)
        casts
        (sizeof-type* t1 t2)
        (sizeof-coercion-of-types t1 t2)
        t1 t2
        (make-timing-loop
         #:timed-action
         (lambda (i acc) `(gunbox guarded-ref))
         #:letrec-bnds
         `([cast-loop : (Int ,t1 -> ,t1)
            (lambda ([n : Int] [g : ,t1])
              (if (= n 0)
                  g 
                  (cast-loop (- n 2) (: g ,t2))))])
         #:let-bnds
         `([guarded-ref : (GRef ,acc-type) ,cl-invocation])
         #:acc-type acc-type
         #:acc-init acc-init
         #:use-acc-action (use-undyned use))))

(define (make-reference-write-timing-loop-record casts depth)
  ;; cadr here subtracts on symbolic repeat
  (define t1 (symbolic-repeat depth 'GRef 'Int))
  (define t2 (symbolic-repeat depth 'GRef 'Dyn))
  (define ref (symbolic-repeat depth 'gbox -22))
  (define use (lambda (v)
                `(+ ,(cadr (symbolic-repeat depth 'gunbox v))
                    ,(symbolic-repeat depth 'gunbox 'guarded-ref))))
  (define-values (acc-type cl-invocation acc-init)
    (if (even? casts)
        (values (cadr t1)
                `(cast-loop ,casts ,ref)
                (cadr (symbolic-repeat depth 'gbox 21)))
        (values (cadr t2)
                `(: (cast-loop ,(sub1 casts) ,ref) ,t2)
                (cadr (symbolic-repeat depth 'gbox '(: 21 Dyn))))))
  (list (format "ref-write-~a-~a" casts depth)
        casts
        (sizeof-type* t1 t2)
        (sizeof-coercion-of-types t1 t2)
        t1 t2
        (make-timing-loop
         #:timed-action
         (lambda (i acc)
           `(begin (gbox-set! guarded-ref acc) acc))
         #:letrec-bnds
         `([cast-loop : (Int ,t1 -> ,t1)
            (lambda ([n : Int] [g : ,t1])
              (if (= n 0)
                  g 
                  (cast-loop (- n 2) (: g ,t2))))])
         #:let-bnds
         `([guarded-ref : (GRef ,acc-type) ,cl-invocation])
         #:acc-type acc-type
         #:acc-init acc-init
         #:use-acc-action use)))


(define reference-read/write-tests
  (for*/list ([casts (in-range 0 6)]
              [depth (in-range 0 6)])
    (cons (make-reference-read-timing-loop-record casts depth)
          (make-reference-write-timing-loop-record casts depth))))

(define reference-read/write-results
  (let ([spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : 42\n$"])
    (for/list ([test (in-list reference-read/write-tests)])
      (match-let ([(cons (list read-name  casts st sc t1 t2 read-prog)
                         (list write-name casts st sc t1 t2 write-prog)) test])
        (define read-src-file (write-source read-name read-prog))
        (define read-name-twosomes-functional
          (string-append read-name "-twosomes-functional"))
        (define read-name-coercions-hybrid
          (string-append read-name "-coercions-hybrid"))
        
        (define write-src-file (write-source write-name write-prog))
        (define write-name-twosomes-functional
          (string-append write-name "-twosomes-functional"))
        (define write-name-coercions-hybrid
          (string-append write-name "-coercions-hybrid"))
        (list
          read-name casts st sc t1 t2
          (compile&run/iteration-time
           #:base-name     read-name-twosomes-functional
           #:src-file      read-src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Twosomes
           #:function-repr 'Functional
           #:output-regexp spec
           #:memory-limit  (* 4096 20000))
          (compile&run/iteration-time
           #:base-name     read-name-coercions-hybrid
           #:src-file      read-src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Coercions
           #:function-repr 'Hybrid
           #:output-regexp spec
           #:memory-limit  (* 4096 20000))
          (compile&run/iteration-time
           #:base-name     write-name-twosomes-functional
           #:src-file      write-src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Twosomes
           #:function-repr 'Functional
           #:output-regexp spec
           #:memory-limit  (* 4096 20000))
          (compile&run/iteration-time
           #:base-name     write-name-coercions-hybrid
           #:src-file      write-src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Coercions
           #:function-repr 'Hybrid
           #:output-regexp spec
           #:memory-limit  (* 4096 20000)))))))

(with-output-to-file
  "partially-typed-reference-read-write.tex"
  #:exists 'replace
  (lambda ()
    (display
     (string-append
      "\\begin{tabular}{| c | c | r | l | r | l |}\n"
      "  \\hline\n"
      "  \\multicolumn{2}{|c|}{} & "
      "    \\multicolumn{2}{c|}{Guarded Read} &"
      "    \\multicolumn{2}{c|}{Guarded Write} \\\\\n"
      "  \\hline\n"
      "  \\multicolumn{2}{|c|}{} & Twosomes & Coercions & Twosomes & Coercions \\\\\n"
      "  \\hline\n"
      "  Casts & Types/Coercions Size & "
      "     \\multicolumn{4}{c|}{Time($ns$)/Interation} \\\\\n"
      "  \\hline\n"))
    (for ([l reference-read/write-results])
      (match-let ([(list name casts st sc t1 t2
                         (list read-tmean  read-tsdev)
                         (list read-cmean  read-csdev)
                         (list write-tmean write-tsdev)
                         (list write-cmean write-csdev))
                   l])
        (printf "  ~a & ~a / ~a & ~a & ~a & ~a & ~a \\\\\n"
                casts
                st sc
                (real->decimal-string read-tmean decimals)
                (real->decimal-string read-cmean decimals)
                (real->decimal-string write-tmean decimals)
                (real->decimal-string write-cmean decimals))
        (display "  \\hline\n")))
    (display "\\end{tabular}\n")))

|#


(define (make-reference-wr-timing-loop-record casts depth)
  ;; cadr here subtracts on symbolic repeat
  (define t1 (symbolic-repeat depth 'GRef 'Int))
  (define t2 (symbolic-repeat depth 'GRef 'Dyn))
  
  (list (format "ref-wr-~a-~a" casts depth)
        casts
        (sizeof-type* t1 t2)
        (sizeof-coercion-of-types t1 t2)
        t1 t2
        (make-reference-wr-timing-loop t1 t2 casts depth))) 

(define reference-wr-tests
  (for*/list ([casts (in-range 0 6)]
              [depth (in-range 0 6)])
    (make-reference-wr-timing-loop-record casts depth)))

(define reference-wr-results
  (let ([spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : 42\n$"])
    (for/list ([test (in-list reference-wr-tests)])
      (match-let ([(list name  casts st sc t1 t2 prog) test])
        (define src-file (write-source name prog))
        (define name-twosomes
          (string-append name "-twosomes-functional"))
        (define name-coercions
          (string-append name "-coercions-hybrid"))
           (match-define (list t-run-time* t-iter-time*)
          (compile&run/iteration-time
           #:base-name     name-twosomes
           #:src-file      src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Twosomes
           #:function-repr 'Functional
           #:output-regexp spec
           #:memory-limit  (* 4096 30000)
           #:mean-of-runs? #f))
        (match-define (list c-run-time* c-iter-time*)
          (compile&run/iteration-time
           #:base-name     name-coercions
           #:src-file      src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Coercions
           #:function-repr 'Hybrid
           #:output-regexp spec
           #:memory-limit  (* 4096 30000)
           #:mean-of-runs?  #f))
        (list
         name casts st sc t1 t2
         (mean-of-runs t-run-time* t-iter-time*)
         (list t-run-time* t-iter-time*)
         (mean-of-runs c-run-time* c-iter-time*)
         (list c-run-time* c-iter-time*))))))

(call-with-output-file
  "partially-typed-reference-write-read.tex"
  #:exists 'replace
  (lambda (tex-out)
    (call-with-output-file
      "partially-typed-reference-write-read-casts=1.txt"
      #:exists 'replace
      (lambda (data-out-casts=1)
        (call-with-output-file
          "partially-typed-reference-write-read-types=2.txt"
          #:exists 'replace
          (lambda (data-out-types=2)
            (display
             (string-append
              "\\begin{tabular}{| c | c | r | r |}\n"
              "  \\hline\n"
              "  \\multicolumn{2}{|c|}{} & Type-Based & Coercions \\\\\n"
              "  \\hline\n"
              "  Casts & Type/Coercion Size & "
              "     \\multicolumn{2}{c|}{Time($ns$)/Interation} \\\\\n"
              "  \\hline\n")
             tex-out)
            (display
             (string-append
              "#casts type coercions twosome-runs twosome-iters "
              "coercions-runs coercion-iters\n")
             data-out-casts=1)
            (display
             (string-append
              "#casts type coercions twosome-runs twosome-iters "
              "coercions-runs coercion-iters\n")
             data-out-types=2)
            (for ([l reference-wr-results])
              (match-let ([(list name casts st sc t1 t2
                                 (list tmean tsdev) (list truns titers)
                                 (list cmean csdev) (list cruns citers)) l])
                (fprintf tex-out
                         "  ~a & ~a / ~a & ~a & ~a \\\\\n"
                         casts
                         st sc
                         (real->decimal-string tmean decimals)
                         (real->decimal-string cmean decimals))
                (display "  \\hline\n" tex-out)
                (when (= casts 1)
                  (for ([tr truns] [ti titers] [cr cruns] [ci citers])
                    (fprintf data-out-casts=1
                             "~a ~a ~a ~a ~a ~a ~a\n"
                             casts st sc tr ti cr ci)))
                (when (= st 2)
                  (for ([tr truns] [ti titers] [cr cruns] [ci citers])
                    (fprintf data-out-types=2
                             "~a ~a ~a ~a ~a ~a ~a\n"
                             casts st sc tr ti cr ci)))))
            (display "\\end{tabular}\n" tex-out)))))))

(system "python partially-typed.py")
(system "python fn-app-by-casts-for-abstract.py")
