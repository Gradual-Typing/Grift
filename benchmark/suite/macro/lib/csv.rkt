#lang racket/base
(require racket/file
         racket/function
         racket/match
         racket/sequence
         racket/string)

(provide (all-defined-out))
;; convert a csv string to a list of lists of strings
(define (string->csv s)
  (define (parse-lines lns) (string-split lns "\n"))
  (define (parse-line  ln) (string-split ln #px"\\s*,\\s*"))
  (map parse-line (parse-lines s)))

(define (file->csv p) (string->csv (file->string p)))

;; print a csv file
(define (csv->string csv)
  (define (join-line rows)
    (string-join rows ","))
  (define (join-lines ls) 
    (string-join ls "\n"))
  (join-lines (map join-line csv)))

(define (string->file str file #:exists [ex 'replace])
  (call-with-output-file file #:exists ex (lambda (p) (display str p))))

(define (csv->file csv file #:exists [ex 'replace])
  (string->file (csv->string csv) file #:exists ex))

(define (csv? x)
  (and (list? x)
       (andmap csv-row? x)
       (or (null? x) 
           (let ([l (length (car x))])
             (andmap (lambda (r) (eqv? (length r) l)) x)))))

(define (csv-row? x) (and (list? x) (andmap string? x)))
(define (index-list? x)
  (and (list? x) (andmap exact-nonnegative-integer? x)))
