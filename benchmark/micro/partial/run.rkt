#lang racket

(require
 "../helpers.rkt"
 "../../helpers.rkt"
 "../code-templates.rkt"
 "../../../src/compile.rkt"
 syntax/location
 racket/runtime-path
 racket/struct
 math)

(define iterations-per-run (make-parameter 1000))
(define runs-of-program (make-parameter 100))
(define recompile-existing-files? (make-parameter #f))
(define cast-representations (make-parameter '(Type-Based Coercions)))


(define (make-directory-unless-it-exists path)
  (unless (directory-exists? path)
    (when (file-exists? path)
      (error 'run.rkt "file exists in place of ~a directory" path))
    (make-directory path)))

(define-runtime-path this-dir ".")

(define-runtime-path src-dir "src")
(make-directory-unless-it-exists src-dir)

(define-runtime-path out-dir "out")
(make-directory-unless-it-exists out-dir)

(define-runtime-path tmp-dir "tmp")
(make-directory-unless-it-exists tmp-dir)

(define (configuration->test-name test casts tsize csize overhead? [cast-rep #f])
  (format "~a_~a_~a_~a_~a~a"
          test casts tsize csize
          (if overhead? 't 'f)
          (if (not cast-rep)
              ""
              (string-append "_" (symbol->string cast-rep)))))

(define (name->configuration name)
  (match (string-split name "_")
    [(list-rest  test-str casts-str tsize-str csize-str overhead? maybe-cast-rep)
     (=> fail-match)
     (values (string->symbol test-str)
             (or (string->number casts-str) (fail-match))
             (or (string->number tsize-str) (fail-match))
             (or (string->number csize-str) (fail-match))
             (case overhead?
               [("t") #t]
               [("f") #f]
               [else (fail-match)])
             (if (null? maybe-cast-rep)
                 #f
                 (string->symbol (car maybe-cast-rep))))]
    [other (error 'test-name->configuration "invalid ~v" name)]))

(define (configuration->file
         base-path extension test casts t-size c-size overhead? [cast-rep #f])
  (define name
    (configuration->test-name test casts t-size c-size overhead? cast-rep))
  (build-path base-path (string-append name extension)))

(define (configuration->src test casts t-size c-size overhead?)
  (configuration->file src-dir ".schml" test casts t-size c-size overhead?))

(define (file->configuration path)
  (define name (path->string (path-replace-suffix (file-name-from-path path) "")))
  (name->configuration name))

(define (configuration->output-file test casts t-size c-size overhead? cast-rep)
  (configuration->file out-dir ".txt" test casts t-size c-size overhead? cast-rep))

(define (generate-benchmarks)
  ;; Casts can cast the same function over and over because the code
  ;; that is running will be the cast code.
  ;; Unfortunately this casts code is going to have unlikely cache performance.
  ;; This could be seen as best case cast behavior for the worst case
  ;; function casts.
  (define (make-function-types/init/use depth)
    (let loop ([depth depth])
      (cond
        [(<= depth 0)
         (values 'Int 'Dyn 42 (lambda (acc) acc))]
        [else
         (let-values ([(t1 t2 init use) (loop (sub1 depth))])
           (define t1^ `(,t1 -> ,t1))
           (define t2^ `(,t2 -> ,t2))
           (define use^ (lambda (acc) (use `(,acc ,init))))
           (define init^ `(lambda ([x : ,t1]) x))
           (values t1^ t2^ init^ use^))])))
  
  (define (generate-fn-cast-timing-loop-source depth n-casts overhead?)
    (unless (and (exact-nonnegative-integer? n-casts)
                 (even? n-casts))
      (error 'cast-timing-loop "no way to loop with odd number of casts"))
    (define-values (t1 t2 init use)
      (make-function-types/init/use depth))
    (define (cast-back-forth acc n)
      (if (zero? n)
          acc
          `(: (: ,(cast-back-forth acc (- n 2)) ,t2) ,t1)))
    (define src-path (configuration->src 'fn-cast
                                         n-casts
                                         (sizeof-type* t1 t2)
                                         (sizeof-coercionof-types t1 t2)
                                         overhead?))
    (write-source
     src-path
     (make-timing-loop
      #:timed-action
      (lambda (i acc)
        (if overhead?
            (cast-back-forth acc 0)
            (cast-back-forth acc n-casts)))
      #:letrec-bnds  `([f : ,t1 ,init])
      #:acc-type t1
      #:acc-init 'f
      #:use-acc-action use))
    src-path)

  (for* ([depth (in-range 2 6)]
         [n-casts (list 2)]
         [overhead? (list #f #t)])
    (generate-fn-cast-timing-loop-source depth n-casts overhead?))
  
  (define (make-app-timing-loop casts t1 t2 init use overhead?)
    (define id-type `(,t1 -> ,t1))
    (define id-type-inv `(,t2 -> ,t2))
    (define-values (id-casted-type acc-type cl-invocation use-undyned)
      (if (even? casts)
          (values id-type t1  
                  `(cast-loop ,casts id)
                  (lambda (use) use))
          (values id-type-inv t2
                  `(: (cast-loop ,(sub1 casts) id) (,t2 -> ,t2))
                  (lambda (use) (lambda (v) `(: ,(use v) Int))))))
    (make-timing-loop
     #:timed-action (lambda (i acc)
                      (if overhead?
                          acc
                          `(id-casted ,acc)))
     #:letrec-bnds
     `([cast-loop : (Int ,id-type -> ,id-type)
                  (lambda ([n : Int] [g : ,id-type])
                    (if (= n 0)
                        g 
                        (cast-loop (- n 2) (: g ,id-type-inv))))]
       [id : ,id-type (lambda ([x : ,t1]) x)])
     #:let-bnds
     `([id-casted : ,id-casted-type ,cl-invocation])
     #:acc-type acc-type
     #:acc-init init
     #:use-acc-action (use-undyned use)))
  


    (define (generate-fn-app-timing-loop-source casts depth overhead?)
      (define-values (t1 t2 init use) (make-function-types/init/use depth))
      (define id-type `(,t1 -> ,t1))
      (define id-type-inv `(,t2 -> ,t2))
      (define type-size (sizeof-type* id-type id-type-inv))
      (define crcn-size (sizeof-coercionof-types id-type id-type-inv))
      (define src-path
        (configuration->src 'fn-app casts type-size crcn-size overhead?))
      (write-source src-path
                    (make-app-timing-loop casts t1 t2 init use overhead?)))
  
    (for* ([casts (in-range 0 15)]
           [depth (in-range 0 4)]
           [overhead? (list #f #t)])
      (generate-fn-app-timing-loop-source casts depth overhead?))

    (define (make-reference-cast-timing-loop t1 t2 depth overhead?)
      (make-timing-loop
       #:timed-action (lambda (i acc) (if overhead? acc `(: (: ,acc ,t2) ,t1)))
       #:acc-type t1
       #:acc-init (symbolic-repeat depth 'gbox 42)
       #:use-acc-action (lambda (v) (symbolic-repeat depth 'gunbox v))))

    (define (symbolic-repeat n ctr base)
      (if (<= n 0)
          `(,ctr ,base)
          `(,ctr ,(symbolic-repeat (sub1 n) ctr base))))
    
    (define (generate-reference-cast-timing-loop depth casts overhead?)
      (define t1 (symbolic-repeat depth 'GRef 'Int))
      (define t2 (symbolic-repeat depth 'GRef 'Dyn))
      (define type-size (sizeof-type* t1 t2))
      (define crcn-size (sizeof-coercionof-types t1 t2))
      (define src-path
        (configuration->src 'ref-cast casts type-size crcn-size overhead?))
      (write-source src-path (make-reference-cast-timing-loop t1 t2 depth overhead?)))

    (for* ([depth (in-range 0 5)]
           [casts (list 2)]
           [overhead? '(#f #t)])
      (generate-reference-cast-timing-loop depth casts overhead?))
    
    (define (generate-reference-write-read-timing-loop depth casts overhead?)
      (define t1 (symbolic-repeat depth 'GRef 'Int))
      (define t2 (symbolic-repeat depth 'GRef 'Dyn))
      (define ref (symbolic-repeat depth 'gbox (if overhead? 21 -22)))
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
      (define type-size (sizeof-type* t1 t2))
      (define crcn-size (sizeof-coercionof-types t1 t2))
      (define src-file
        (configuration->src 'ref-write-read casts type-size crcn-size overhead?))

      (write-source
       src-file
       (make-timing-loop
        #:timed-action
        (lambda (i acc)
          (if overhead?
              acc
              `(begin
                 (gbox-set! guarded-ref ,acc)
                 (gunbox guarded-ref))))
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
    
    (for* ([depth (in-range 0 5)]
           [casts (in-range 0 15)]
           [overhead? '(#f #t)])
      (generate-reference-write-read-timing-loop depth casts overhead?)))

#|
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


;|#

(define (compile-benchmarks)
  (for* ([src-path (in-directory src-dir (thunk* #f))]
         [cast-rep (in-list (cast-representations))])
    (define-values (test casts type-size crcn-size overhead? _)
      (file->configuration src-path))
    (define exe-path
      (configuration->file tmp-dir "" test casts
                           type-size crcn-size overhead? cast-rep))

    ;; if the file-exists and we are not recompiling then
    ;; don't compile the file.
    (unless (and (not (recompile-existing-files?))
                 (file-exists? exe-path))
      (define c-path
        (configuration->file
         tmp-dir ".c"
         test casts type-size crcn-size overhead? cast-rep))
      (define s-path
        (configuration->file
         tmp-dir ".s"
         test casts type-size crcn-size overhead? cast-rep))
      
      (printf "compiling: ~a\n\t" (file-name-from-path exe-path))
      
      (time
       (compile src-path
                #:output exe-path
                #:keep-c c-path
                #:keep-s s-path
                #:cast cast-rep
                #:cc-opts "-O3 -w")))))

(define (run-benchmarks)
  (for ([path (in-directory tmp-dir (thunk* #f))])

    ;; Assumes only the execuatables have no extenstion
    (unless (filename-extension path)
      (define-values (test casts type-size crcn-size overhead? cast-rep)
        (file->configuration path))
      
      (define test-output-file
        (configuration->file tmp-dir ".txt"
                             test casts type-size crcn-size overhead? cast-rep))

      (define run-input-file
        (configuration->file tmp-dir ".in"
                             test casts type-size crcn-size overhead? cast-rep))

      (with-output-to-file run-input-file #:exists 'replace
        (thunk (print (iterations-per-run))))
      
      (printf "running ~a\n" (file-name-from-path path))
      
      (call-with-output-file test-output-file
        #:exists 'replace
        (lambda (p)
          (for ([i (in-range 0 (runs-of-program))])
            (parameterize ([current-output-port p])
              (with-input-from-file run-input-file
                (lambda ()
                  (unless (system* path)
                    (flush-output p)
                    (error 'run-benchmarks
                           "subprocess returned error code")))))))))))

(define (unit->nano m)
  (* m (expt 10 9)))

(define (analyze-benchmarks)
  (struct result (name casts type-size crcn-size
                       overhead? cast-rep mean sdev runs) #:prefab)
  (define (benchmark-output-file? x)
    (equal? (filename-extension x) #"txt"))
  (define benchmark-results
    (for/list ([output-file (find-files benchmark-output-file? tmp-dir)])
      (define-values (test casts t-size c-size overhead? cast-rep)
        (file->configuration output-file))

      (define output (file->string (simple-form-path output-file)))

      (define pattern #px"time \\(sec\\): (\\d+.\\d+)\nInt : 42\n")
      (define results
        (map string->number (regexp-match* pattern output #:match-select cadr)))

      (define mean-runtime (mean results))
      (define sdev-runtime (stddev results))
      (result test casts t-size c-size overhead?
              cast-rep mean-runtime sdev-runtime results)))

  (define ((result=? r1) r2)
    #;(printf "r1 ~a r2 ~a\n" r1 r2)
    ((r=? r1) r2))
  
  (define ((r=? r1) r2)
    (match* (r1 r2)
      [((cons a1 r1*) (cons a2 r2*))
       (and (or (eq? '_ a1) (eqv? a1 a2)) ((result=? r1*) r2*))]
      [((list) _) #t]
      [((struct result _) _)
       ((result=? (struct->list r1)) r2)]
      [(_ (struct result _))
       ((result=? r1) (struct->list r2))]
      [(_ _) (error 'result=? "invalid input:" r1 r2)]))
  
  ;; Fn Cast Analysis
  ;; subtract average empty-loop (overhead) from each runs time to get time spent in loop
  ;; scale each loop to nanoseconds
  ;; devide by the number of iterations performed in order to find the time per iterations
  (define (cast-analysis test-name cast-rep)
    (with-output-to-file
      (build-path out-dir (format "~a_~a.txt" test-name cast-rep))
      #:exists 'replace
      (lambda ()
        (define mean-sdev-map
          (map (lambda (r)
                 (list (result-type-size r)
                       (result-mean r)
                       (result-sdev r)))
               (filter (result=? `(,test-name 2 _ _ #t ,cast-rep))
                       benchmark-results)))
        (display
         (string-append
          (format "# partially typed function casts ~a\n" cast-rep)
          (apply
           string-append
           (for/list ([overhead-result mean-sdev-map])
             (match-let ([(list ts m s) overhead-result])
               (format "# empty-loop type-size: ~a mean: ~a sdev: ~a\n"
                       ts (unit->nano m) (unit->nano s))))) 
          "# sizeof-types sizeof-coercions time(ns)/cast\n"))
        (for* ([rslt (filter (result=? `(,test-name 2 _ _ #f ,cast-rep))
                             benchmark-results)])
          (match rslt
            [(result _ _ type-size crcn-size _ _ _ _ runs)
             ;; This should be printed into the benchmark log file and saved in the
             ;; results
             (define iters (iterations-per-run))
             (define mean-overhead  (cadr (assoc type-size mean-sdev-map)))
             (for ([run runs])
               (printf "~a ~a ~a\n"
                       type-size crcn-size
                       (/ (unit->nano (- run mean-overhead)) iters)))]
            [other (void)])))))
  (cast-analysis 'fn-cast 'Type-Based)
  (cast-analysis 'fn-cast 'Coercions)
  (cast-analysis 'ref-cast 'Type-Based)
  (cast-analysis 'ref-cast 'Coercions)

  ;; Fn app analysis
  (define (use-analysis test-name cast-slice crcn-slice cast-rep)
    (define overhead-mean-map
      (make-immutable-hash
       (map (lambda (r)
             (cons (list (result-casts r)
                         (result-type-size r)
                         (result-crcn-size r))
                   (list (result-mean r)
                         (result-sdev r))))
           (filter (result=? `(,test-name _ _ _ #t ,cast-rep))
                   benchmark-results))))
    (define data-file-header
      (string-append
       (apply string-append
              (for/list ([(k v) (in-hash overhead-mean-map)])
                (match-let ([(list c ts cs) k]
                            [(list m s) v])
                  (format
                   "# ~a ~a ~a overhead run mean: ~a app mean: ~a sdev: ~a \n"
                   c ts cs m (/ m (iterations-per-run)) s))))
       "# Casts types coercions run-time app-time "))
    (call-with-output-file
      (build-path
       out-dir
       (format "~a-~a-coercions=~a.txt" test-name cast-rep crcn-slice))
      #:exists 'replace
      (lambda (coercion-p)
        (call-with-output-file
          (build-path
           out-dir
           (format "~a-~a-casts=~a.txt" test-name cast-rep cast-slice))
          #:exists 'replace
          (lambda (cast-p)
            (display data-file-header coercion-p)
            (display data-file-header cast-p)
            ;; This way of processing the data stinks but I am doing
            ;; it because I am in a time crunch right now!
            ;; TODO come up with a simpler analisis that is easier to modify
            (for ([r (filter (result=? `(,test-name _ _ _ #f ,cast-rep))
                             benchmark-results)])
              
              (match-let* ([(result _ casts st sc _ _ _ _ runs) r])
                (define ports
                  (cond
                    [(and (= casts cast-slice) (= sc crcn-slice))
                     (list cast-p coercion-p)]
                    [(= casts cast-slice) (list cast-p)]
                    [(= sc crcn-slice) (list coercion-p)]
                    [else '()]))
                
                (unless (null? ports)
                  (match-define (list o-mean o-sdev)
                    (hash-ref overhead-mean-map (list casts st sc)))

                  (for ([run runs])
                    (define run-no-o (- run o-mean))
                    (define run-ns (unit->nano run-no-o))
                    (define iter-ns (/ run-ns (iterations-per-run)))
                    (define line
                      (format "~a ~a ~a ~a ~a\n"
                              casts st sc run-ns iter-ns))
                    (for ([port ports])
                      (display line port)))))))))))

  (for* ([test-name `(fn-app ref-write-read)]
         [casts (list 1)]
         [crcns (list 15)]
         [repr  '(Type-Based Coercions)])
    (use-analysis test-name casts crcns repr))

  (pretty-print (map struct->list benchmark-results))

  (parameterize ([current-directory this-dir])
    (system "python plot.py")))

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

#|
;; Runs = the number of times the test is repeated in order to understand
;; how much volitility is coming from the system it is being run o
(define iters 1000)
(define runs  100)
(define decimals 2)
(define prec `(= ,decimals))
|#

#|

(define function-cast-results
  (let ([spec #px"^time \\(sec\\): (\\d+.\\d+)\nInt : 42\n$"])
    (for/list ([test (in-list function-cast-tests)])
      (match-let ([(list name depth args st sc t1 t2 prog) test])
        
        ))))

(call-with-output-to-file
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
;;|#


(define (clean-benchmarks)
  (delete-directory/files src-dir)
  (delete-directory/files out-dir)
  (delete-directory/files tmp-dir))

(module+ main
  (define program-main
    (make-parameter
     (lambda ()
       (generate-benchmarks)
       (compile-benchmarks)
       (run-benchmarks)
       (analyze-benchmarks))))
  (init-heap-kilobytes (* 6 (expt 10 6)))
  (command-line
   #:once-each
   [("-i" "--iterations") number-of-iterations
    "number of iterations in timing loop"
    (iterations-parameter
     (or (string->exact-integer number-of-iterations)
         (error 'dynamic-write-read-benchmark "bad iterations argument")))]
   [("-r" "--runs") number-of-runs
    "number of trials of each benchmark"
    (runs-parameter
     (or (string->exact-integer number-of-runs)
         (error 'dynamic-write-read-benchmark "bad runs argument")))]
   [("-m" "--memory") memory-start-size
    "size in kilobytes of schml starting memory"
    (init-heap-kilobytes
     (or (string->exact-integer memory-start-size)
         (error 'dynamic-write-read-benchmark "bad memory argument")))]
   [("-e" "--epsilon") epsilon
    "smallest time in milliseconds that will be recorded without error"
    (epsilon-parameter
     (or (string->number epsilon)
         (error 'dynamic-write-read-benchmark "bad epsilon argument")))]
   #:once-any
   ["--test" "run a quick check to see if all generated programs compile"
    (runs-of-program 1)]
   ["--generate" "Generate schml programs for benchmark."
    (program-main generate-benchmarks)]
   ["--compile" "Run schml on all programs in the src dir."
    (program-main compile-benchmarks)]
   ["--run" "Run programs in exe directory"
    (program-main run-benchmarks)]
   ["--analyze" "Run programs in exe directory"
    (program-main analyze-benchmarks)]
   ["--clean" "delete working files"
    (program-main clean-benchmarks)]
   #:args a ((program-main))))

