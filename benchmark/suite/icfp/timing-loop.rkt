#lang racket

(require
 "helpers.rkt"
 "code-templates.rkt"
 syntax/location)

(initialize-dirs! (file-name-from-path (quote-source-file)))

;; Runs = the number of times the test is repeated in order to understand
;; how much volitility is coming from the system it is being run o
(define iters 1000)
(define runs  100)
(define spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : \\d+\n$")


(define (mk-fn-app-loop n)
  (make-timing-loop
   #:letrec-bnds (for/list ([i (range 1 (add1 n))])
                   `[,(string->symbol (format "add~a" i))
                     : (Int -> Int)
                     (lambda ([x : Int]) (+ x ,i))])
   #:acc-type       'Int 
   #:acc-init       0
   #:timed-action   (lambda (i acc)
                      (let loop ([i n])
                        (if (= 0 i)
                            acc
                            `(,(string->symbol (format "add~a" i)) ,(loop (sub1 i))))))
   #:use-acc-action (lambda (acc) acc)))

(define results
  (for/list ([i (in-range 1 100)])
    (define base (format "loop-fn-app-~a" i))
    (define src-file
      (write-source base (mk-fn-app-loop i)))
    (list i
          (compile&run/iteration-time
           #:base-name     (string-append base "-twosomes")
           #:src-file      src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Twosomes
           #:function-repr 'Functional
           #:output-regexp spec
           #:mean-of-runs? #f)
          (compile&run/iteration-time
           #:base-name     (string-append base "-coecions")
           #:src-file      src-file
           #:runs          runs
           #:iterations    iters
           #:cast-repr     'Coercions
           #:function-repr 'Hybrid
           #:output-regexp spec
           #:mean-of-runs? #f))))


(with-output-to-file
  "loop-fn-app.txt"
  #:exists 'replace
  (lambda ()
    (for ([result (in-list results)])
      (match-let ([(list apps (list trun titer) (list crun citer)) result])
        (for ([ttime (in-list titer)]
              [ctime (in-list citer)])
          (printf  "~a\t~a\t~a\n" apps ttime ctime))))))

(system "python timing-loop.py")

