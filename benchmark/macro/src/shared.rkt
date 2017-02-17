#lang typed/racket

(require racket/runtime-path csv-reading)
(require/typed csv-reading
  [csv->list ((-> (Listof String)) -> (Listof (Listof String)))]
  [make-csv-reader (String (Listof (Pair Symbol Any)) -> (-> (Listof String)))])

(provide (all-defined-out))

(define-runtime-path root-dir ".")
(define-runtime-path stats-dir "stats")

(define (parse-runtimes [in : String]
			[pat : PRegexp #px"time \\(sec\\) (\\d+.\\d+)"])
 : (Listof Float)			
 (match (regexp-match* pat in)
  [(list match* ...)
   (for/list ([m match*])
    (match m
     [(pregexp pat (list _ time-str))
      (define num?
       (if (not time-str)
       	   (error 'invalid "~a" m)
       	   (string->number time-str)))
      (unless (and num? (real? num?))
        (error 'parse-runtimes "invalid: ~a" time-str))
      (real->double-flonum num?)]
     [other (error 'unmatched "~a" other)]))]
  [_ (error 'parse-runtimes "invalid:\n~a" in)]))

(define (make-containing-directory [path : Path])
  (define-values (base file _) (split-path path))
  (cond
    [(not base) (void)]
    [(eq? 'relative base) (void)]
    [(directory-exists? base) (void)]
    [else (make-directory* base)]))

(: uniques (All (A) (Listof A) -> (Listof A)))
(define (uniques xs) (set->list (list->set xs)))

(: run-gnuplot-script : Symbol (Listof (Pair Symbol Any)) -> Void)
(define (run-gnuplot-script script args)
  (define args-str
    (with-output-to-string
      (lambda ()
        (for ([a args])
          (printf "~a=~v;" (car a) (cdr a))))))
  (define cmd (format "gnuplot -e ~v ~a" args-str script))
  (unless (system cmd)
    (error 'run-gnuplot-script "failed: ~a" cmd)))

(: file->stats-table : Path -> (HashTable String (List Float Float)))
(define (file->stats-table path)
  (define lines
    (csv->list
     (make-csv-reader
      (file->string path)
      '((comment-chars #\#)
        (strip-leading-whitespace? . #t)
        (strip-trailing-whitespace? . #t)))))
  (make-immutable-hash
   (for/list : (Listof (List String Float Float))
             ([line lines])
     (match line
       [(list lang mean-str sdev-str)
        (define mean? (string->number mean-str))
	(define sdev? (string->number sdev-str))
        (if (and (real? mean?) (real? sdev?))
            (list lang
                  (real->double-flonum mean?)
                  (real->double-flonum sdev?))
            (error 'invalid "~a" line))]))))

(: general-stats-path : String -> Path)
(define (general-stats-path benchmark)
  (build-path stats-dir (string-append benchmark ".csv")))

(define (stats-mean-time-ref [benchmark : String]
                             [impl : String]
                             [unfound : (-> Float)])
  : Float
  (define table (file->stats-table (general-stats-path benchmark)))
  (car (hash-ref table impl (lambda () (cons (unfound) 0.0)))))

(: list-of-lists->csv (->* ((Listof (Listof Any))) (Output-Port) (U #f String)))
(define (list-of-lists->csv lol [p #f])
  (cond
    [p
     (for ([l (in-list lol)])
       (match l
         [(list) (void)]
         [(list f) (newline p) (display f p)]
         [(list f r ...)
          (newline p)
          (display f p)
          (for ([e (in-list r)])
            (display "," p)
            (display e p))]
         [other (error 'list-of-lists->csv "invalid: ~a" other)]))
     #f]
    [else
     (call-with-output-string
      (lambda (p) (list-of-lists->csv lol p)))]))
