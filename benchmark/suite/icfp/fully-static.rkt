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

(define fn-app-test
  (make-timing-loop
   #:letrec-bnds '([add1 : (Int -> Int)
                         (lambda ([x : Int]) (+ x 1))])
   #:acc-type       'Int 
   #:acc-init       0
   #:timed-action   (lambda (i acc) `(add1 acc))
   #:use-acc-action (lambda (acc) acc)))

(define fn-app-src-file
  (write-source "fully-static-fn-app" fn-app-test))

(define-values (fn-app-twosomes fn-app-coercions)
  (values
   (compile&run/iteration-time
    #:base-name     "fully-static-fn-app-twosomes"
    #:src-file      fn-app-src-file
    #:runs          runs
    #:iterations    iters
    #:cast-repr     'Twosomes
    #:function-repr 'Functional
    #:output-regexp spec)
   (compile&run/iteration-time
    #:base-name     "fully-static-fn-app-coercions"
    #:src-file      fn-app-src-file
    #:runs          runs
    #:iterations    iters
    #:cast-repr     'Coercions
    #:function-repr 'Hybrid
    #:output-regexp spec)))


(define ref-write-read-test
  (make-timing-loop
   #:letrec-bnds  '([read  : ((GRef Int) -> Int)
                           (lambda ([r : (GRef Int)])
                             (gunbox r))]
                    [write : ((GRef Int) Int -> ())
                           (lambda ([r : (GRef Int)] [v : Int])
                             (gbox-set! r v))])
   #:let-bnds     '([ref : (GRef Int) (gbox 0)])
   #:acc-type     'Int 
   #:acc-init     0
   #:timed-action (lambda (i acc)
                    `(begin
                       (write ref (+ ,acc 1))
                       (read ref)))
   #:use-acc-action (lambda (acc) acc)))

 ;; (define ref-write-read-test
 ;;  (make-reference-wr-timing-loop '(GRef Int) '(GRef Dyn) 0 0))

(define ref-write-read-src-file
  (write-source "fully-static-ref-write-read" ref-write-read-test))

(define-values (ref-write-read-twosomes ref-write-read-coercions)
  (values
   (compile&run/iteration-time
    #:base-name     "fully-static-ref-write-read-twosomes"
    #:src-file      ref-write-read-src-file
    #:runs          runs
    #:iterations    iters
    #:cast-repr     'Twosomes
    #:function-repr 'Functional
    #:output-regexp spec)
   (compile&run/iteration-time
    #:base-name     "fully-static-ref-write-read-coercions"
    #:src-file      ref-write-read-src-file
    #:runs          runs
    #:iterations    iters
    #:cast-repr     'Coercions
    #:function-repr 'Hybrid
    #:output-regexp spec)))

(define fn-app-twosomes-c-src
  (build-path src-dir "fully-static-fn-app-twosomes.c"))
(define fn-app-coercions-c-src
  (build-path src-dir "fully-static-fn-app-coercions.c"))
(define ref-write-read-twosomes-c-src
  (build-path src-dir "fully-static-ref-write-read-twosomes.c"))
(define ref-write-read-coercions-c-src
  (build-path src-dir "fully-static-ref-write-read-coercions.c"))

(unless (and
         (file-exists? fn-app-twosomes-c-src)
         (file-exists? fn-app-coercions-c-src)
         (file-exists? ref-write-read-twosomes-c-src)
         (file-exists? ref-write-read-coercions-c-src)
         (begin
           (display "Use existing files? [#t/#f]:")
           (read)))
  (copy-file (build-path tmp-dir "fully-static-fn-app-twosomes.c")
             fn-app-twosomes-c-src
             #t)
  (copy-file (build-path tmp-dir "fully-static-fn-app-coercions.c")
             fn-app-coercions-c-src
             #t)
  (copy-file (build-path tmp-dir "fully-static-ref-write-read-twosomes.c")
             ref-write-read-twosomes-c-src
             #t)
  (copy-file (build-path tmp-dir "fully-static-ref-write-read-coercions.c")
             ref-write-read-coercions-c-src
             #t)
  (begin
    (display "Edit files and press any key to run tests")
    (read)))

(define-values (fn-app-twosomes-c fn-app-coercions-c)
  (values
   (compile-c/run #:base-name "fully-static-fn-app-twosomes-c"
                  #:src-file  fn-app-twosomes-c-src
                  #:runs      runs
                  #:iters     iters
                  #:out-rx    spec)
   (compile-c/run #:base-name "fully-static-fn-app-coercions-c"
                  #:src-file  fn-app-coercions-c-src
                  #:runs      runs
                  #:iters     iters
                  #:out-rx    spec)))

(define-values (ref-write-read-twosomes-c ref-write-read-coercions-c)
  (values
   (compile-c/run #:base-name "ref-write-read-twosomes-c"
                  #:src-file  ref-write-read-twosomes-c-src
                  #:runs      runs
                  #:iters     iters
                  #:out-rx    spec)
   (compile-c/run #:base-name "ref-write-read-coercions-c"
                  #:src-file  ref-write-read-coercions-c-src
                  #:runs      runs
                  #:iters     iters
                  #:out-rx    spec)))



(define-values (results)
  (list (list "Function Application" 
              (list "Type-Based"
                    fn-app-twosomes
                    fn-app-twosomes-c)
              (list "Coercions"
                    fn-app-coercions
                    fn-app-coercions-c))
        (list "Reference Read and Write"
              (list "Type-Based"
                    ref-write-read-twosomes
                    ref-write-read-twosomes-c)
              (list "Coercions"
                    ref-write-read-coercions
                    ref-write-read-coercions-c))))

(with-output-to-file
  "fully-static-results.tex"
  #:exists 'replace
  (lambda ()
    (display
     (string-append
      "\\begin{tabular}{| l | r | r | r | r | r |}\n"
      "\\hline\n"
      " & \\multicolumn{2}{c|}{Grift}"
      "        & \\multicolumn{2}{c|}{C Edit} & \\\\\n"
      "\\cline{2-6}\n"
      "\\multicolumn{1}{|c|}{all units in $ns$} & time & stdev &"
      "       time & stdev & overhead \\\\\n"
      "\\hline\n")) 
    (for ((r (in-list results)))
      (match-define (list n b* ...) r)
      (display
       (string-append "\\multicolumn{6}{|l|}{ " n " }\\\\\n\\hline\n"))
      (for ((b (in-list b*)))
        (match-define `(,r (,s-mean ,s-sdev) (,c-mean ,c-sdev)) b)
        (define percent-overhead (exact-round (* 100 (/ (- s-mean c-mean) c-mean))))
        (printf "~a & ~a & ~a & ~a & ~a & ~a\\% \\\\\n\\hline\n"
                r
                (~r s-mean #:precision '(= 2))
                (~r s-sdev #:precision '(= 2))
                (~r c-mean #:precision '(= 2))
                (~r c-sdev #:precision '(= 2))
                percent-overhead)))
    (display "\\end{tabular}\n")))

