#lang racket
;; Casts can cast the same function over and over because the code
;; that is running will be the cast code.
;; Unfortunately this casts code is going to have unlikely cache performance.
;; This could be seen as best case cast behavior.
(require
 "helpers.rkt"
 "code-templates.rkt"
 syntax/location
 math)



;; How many times the timing loop iterates
(define iters (make-parameter 500))
;; How many times each test is run
(define runs  (make-parameter 30))
;; How many variations to figure out only the cost of
;; code to be measured
(define var-count (make-parameter 5))
;; Where to start the variations
(define var-start (make-parameter 2))
;; step from one variation to the next number -> number
(define var-step (make-parameter (lambda (x) (+ 4 x))))
;; how much memory the program starts with
(define memory-limit (make-parameter (* 4096 30000)))

;; Numeric presentation
;; number of decimals
(define decimals (make-parameter 2))
;; significant decimal show as 0 instead
(define prec `(= ,decimals))


(define (make-function-types/init/use depth)
  (let loop ([depth depth])
    (cond
      [(<= depth 0)
       (values
        '(Int -> Int)
        '(Dyn -> Dyn)
        '(lambda ([x : Int]) x)
        (lambda (f) `(,f 42)))]
      [else
       (let-values ([(t1 t2 init use) (loop (sub1 depth))])
         (define t1^ `(,t1 -> ,t1))
         (define t2^ `(,t2 -> ,t2))
         (define use^ (lambda (f) (use `(,f ,init))))
         (define init^ `(lambda ([x : ,t1]) x))
         (values t1^ t2^ init^ use^))])))

(define (cast-back-forth acc n t1 t2)
    (if (zero? n)
        acc
        `(: (: ,(cast-back-forth acc (- n 2) t1 t2) ,t2) ,t1)))

;; Helper that builds function and types

  ;; Helper that causes repreated casts
  

  ;; Helper that 
(define (make-cast-timing-loop/name/size depth n-casts)
  (unless (and (exact-nonnegative-integer? n-casts)
               (even? n-casts))
    (error 'cast-timing-loop "no way to loop with odd number of casts"))
  (define-values (t1 t2 init use)
    (make-function-types/init/use depth))
  (values (format "fn-cast-~a-~a" depth n-casts)
          (sizeof-type* t1 t2)
          (sizeof-coercion-of-types t1 t2)
          (make-timing-loop
           #:timed-action
           (lambda (i acc)
             (cast-back-forth acc n-casts t1 t2))
           #:letrec-bnds  `([f : ,t1 ,init])
           #:acc-type t1
           #:acc-init 'f
           #:use-acc-action use)))

(define spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : 42\n$")



(define (gen-stream f x)
  (stream-cons x (gen-stream f (f x))))

(define (unfold stop? seed->elem seed->next seed)
  (let loop ([recur seed])
    (if (stop? seed)
        '()
        (cons (seed->elem seed) (loop (seed->next seed))))))

;; sum of least squares regression
(define (linear-regression xs ys)
  (define-values (mx my) (values (mean xs) (mean ys)))
  
  ;; slope of the model
  (define a
    (let ([n (for/sum ([x (in-list xs)] [y (in-list ys)])
               (* (- x mx) (- y my)))]
          [d (for/sum ([x (in-list xs)]) (expt (- x mx) 2))])
      (/ n d)))
  
  ;; y intercept of the model
  (define b (- my (* mx a)))
  
  ;; calculate the model coeficient of determination
  ;; represents the amount of the variation in y that the
  ;; model says is accounted for by variation in x.
  (define (model x) (+ (* a x) b))
  (define (residual x y) (- y (model x)))
  (define ss-total
    (for/sum ([y (in-list ys)])
      (expt (- y my) 2)))
  (define ss-residual
    (for/sum ([x (in-list xs)] [y (in-list ys)])
      (expt (residual x y) 2)))

  ;; coefficient of determination 
  (define r2 (- 1 (/ ss-total ss-residual)))
  
  (values a b r2 mx my))

(module+ main
  
  (define casts
    (let loop ([c 0] [n (var-start)])
      (if (= c (var-count))
          '()
          (cons n (loop (add1 c) ((var-step) n))))))
  
  (define function-cast-results
    ;; Whole test generated and stored so that it can be written to
    ;; a file at once.
    (for/list ([depth (in-range 0 5)])
      (display 'depth-for-loop)
      (define depth-results
        (for/list ([num-casts (in-list casts)])
          (display 'num-casts-for-loop)
          (define-values (name size-types size-coercions prog-exp)
            (make-cast-timing-loop/name/size depth num-casts))
          (define-values (src-file name-twosomes name-coercions)
            (values (write-source name prog-exp)
                    (string-append name "-twosomes-functional")
                    (string-append name "-coercions-hybrid")))
          (match-define (list t-run-time* t-iter-time*)
            (compile&run/iteration-time
             #:base-name     name-twosomes
             #:src-file      src-file
             #:runs          (runs)
             #:iterations    (iters)
             #:cast-repr     'Twosomes
             #:function-repr 'Functional
             #:output-regexp spec
             #:memory-limit  (memory-limit)
             #:mean-of-runs? #f))
          (match-define (list c-run-time* c-iter-time*)
            (compile&run/iteration-time
             #:base-name     name-coercions
             #:src-file      src-file
             #:runs          (runs)
             #:iterations    (iters)
             #:cast-repr     'Coercions
             #:function-repr 'Hybrid
             #:output-regexp spec
             #:memory-limit  (memory-limit)
             #:mean-of-runs?  #f))

          (list name depth num-casts size-types size-coercions
                (mean-of-runs t-run-time* t-iter-time*)
                (list t-run-time* t-iter-time*)
                (mean-of-runs c-run-time* c-iter-time*)
                (list c-run-time* c-iter-time*))))

      ;; super ugly append of iter times 
      (define-values (xs twosome-ys coercion-ys size-coercion)
        (for/fold ([xs '()] [t-ys '()] [c-ys '()] [size-coercion (void)])
                  ([results (in-list depth-results)])
          (match-define (list _ _ x _ sc _ (list _ t-ys^) _ (list _ c-ys^))
            results)
          (for/fold ([xs xs] [t-ys t-ys] [c-ys c-ys] [sc sc])
                    ([ty (in-list t-ys^)]
                     [cy (in-list c-ys^)])
            (values (cons x xs) (cons ty t-ys) (cons cy c-ys) sc))))
      
      
      (define-values (t-a t-b t-r2 t-mx t-my)
        (linear-regression xs twosome-ys))
      (define-values (c-a c-b c-r2 c-mx c-my)
        (linear-regression xs coercion-ys))
      
      (define (print-diagnostic-stats a b r2 mx my xs ys)
        (define ro2 (expt (correlation/means mx my xs ys) 2))
        (printf "a=~a b=~a r2=~a ro2=~a\n" a b r2 ro2))
      
      (display "twosomes ")
      (print-diagnostic-stats t-a t-b t-r2 t-mx t-my xs twosome-ys)
      (display "coercions ")
      (print-diagnostic-stats c-a c-b c-r2 c-mx c-my xs coercion-ys)
      
      (list size-coercion
            (list t-a t-b t-r2 t-mx t-my)
            (list c-a c-b c-r2 c-mx c-my)
            depth-results)))

  (match-define (list (list s
                            (list t-a t-b t-r2 t-mx t-my)
                            (list c-a c-b c-r2 c-mx c-my)
                            extra-results) ...)
    function-cast-results)
  

  (define-values (ft-a ft-b ft-r2 ft-mx ft-my)
    (linear-regression s t-a))
  (define-values (fc-a fc-b fc-r2 fc-mx fc-my)
    (linear-regression s c-a))
  
  (printf "final twosomes a=~a b=~a" ft-a ft-b)
  (printf "final coercion a=~a b=~a" fc-a fc-b)

  (with-output-to-file
    "partially-typed-function-cast.txt"
    #:exists 'replace
    (lambda ()
      (display
       (string-append
        "# partially typed function casts twosomes\n"
        "# sizeof-types sizeof-coercions time(us)/iteration\n"))
      (for ([l function-cast-results])
        (match-let ([`(,s (,t-a ,t-o ...) (,c-a ,c-o ...) ,o) l])
          (printf "~a ~a ~a\n" s t-a c-a)))))

  (system "python partially-typed-fn-cast.py"))



 
  ;; (with-output-to-file
  ;;   "partially-typed-general-statistics.txt"
  ;;   #:exists 'replace
  ;;   (lambda ()
  ;;     (define-values (titers)
  ;;       (for/fold ([i '()])
  ;;                 ([l function-cast-results])
  ;;         (match-let ([(list name depth args st sc t1 t2
  ;;                            (list tmean tsdev) (list truns titers)
  ;;                            (list cmean csdev) c-points) l])
  ;;           (append titers i))))
  ;;     (printf "twosomes count: ~a\n" (length titers))
  ;;     (printf "fn cast twosomes total time(us): ~a\n" (sum titers))
  ;;     (printf "fn cast twosomes mean  time(us): ~a\n" (mean titers))
  ;;     (printf "fn cast twosomes sdev(us):       ~a\n" (stddev titers))))

;; (with-output-to-file
;;   "partially-typed-function-cast.tex"
;;   #:exists 'replace
  ;;   (lambda ()
  ;;     (display
  ;;      (string-append
  ;;       "\\begin{tabular}{| c | r | r |}\n"
  ;;       "  \\hline\n"
  ;;       "  & Type-Based & Coercions \\\\\n"
  ;;       "  \\hline\n"
  ;;       "  Type/Coercion Size & "
  ;;       "\\multicolumn{2}{c|}{Time($n s$)/Iteration} \\\\\n"
  ;;       "  \\hline\n"))
  ;;     (for ([l function-cast-results])
  ;;       (match-let ([(list name depth args st sc t1 t2
  ;;                          (list tmean tsdev) t-points
  ;;                          (list cmean csdev) c-points) l])
  ;;         (printf "  ~a / ~a & ~a & ~a \\\\\n"
  ;;                 st sc
  ;;                 (~r tmean #:precision prec)
  ;;                 (~r cmean #:precision prec))
  ;;         (display "  \\hline\n")))
  ;;     (display "\\end{tabular}\n")))


 
