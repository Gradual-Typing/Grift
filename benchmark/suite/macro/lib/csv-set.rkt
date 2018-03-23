#lang racket

;; This program allows you to compare two files of floating point numbers for
;; agreement up to a given delta.

(require csv-reading "../../../config_str.rkt")

(define (map-col csv col p)
  (define (f row)
    (define-values (head tail) (split-at row col))
    (append head (list (p (car tail))) (cdr tail)))
  (map f csv))

(define (s->n s) 
  (define n (string->number s))
  (unless n
    (error 's->n "expected number got ~v" s))
  n)

(define (sort-by-cols csv c.i*)
  (for/fold ([csv csv]) ([c.i? (in-list (reverse c.i*))])
    (match-define (cons c i?) c.i?)
    (cond
      [i?
       (define (key x) (s->n (list-ref x c)))
       (sort csv < #:key key #:cache-keys? #t)]
      [else
       (define (key x) (list-ref x c))
       (sort csv string<? #:key key #:cache-keys? #f)])))
  

(module+ main
  (require racket/cmdline)
  (define who 'csv-set.rkt)
  (define csv-spec
    '((strip-leading-whitespace? . #t)
      (strip-trailing-whitespace? . #t)))
  (define *open-output!* (make-parameter (lambda () (current-output-port))))
  (define *adds* (make-parameter '()))
  (define *sort-result* (make-parameter #f))
  (define *config->name* (make-parameter #f))
  (command-line
   #:multi
   [("-a" "--add") row/col
    "Add a row (or collum) to a csv, but always use row syntax"
    (*adds* (cons row/col (*adds*)))]
   #:once-each
   [("--config-names") 
    col
    "convert configuration number to string"
    (*config->name* (s->n col))]
   [("-i" "--in") in-file
    "the input csv"
    (define p (string->path in-file))
    (unless (and (path? p) (file-exists? p))
      (error who "file ~a not found" in-file))
    (current-input-port (open-input-string (file->string p)))]
   [("-o" "--out") out-file
    "file io"
    (define p (string->path out-file))
    (unless (path? p)
      (error who "invalid file ~a" out-file))
    (*open-output!* (lambda () (open-output-file p #:exists 'replace)))]
   #:multi
   [("--si" "--sorti") col
    "sort result by column interpreted as racket number"
    (define n (string->number col))
    (unless n (error who "expected number: ~a" col))
    (define s (*sort-result*))
    (*sort-result* `((,n . #t) . ,(if s s '())))]
   [("--su" "--sortu") col
    "sort result by column as a string"
    (define n (string->number col))
    (unless n (error who "expected number: ~a" col))
    (define s (*sort-result*))
    (*sort-result* `((,n . #f) . ,(if s s '())))]
   #:args ()
   (define csv (make-csv-reader (current-input-port) csv-spec))
   (define input-csv-set (list->mutable-set (csv->list csv)))
   (for ([row-str (in-list (*adds*))])
     (match (csv->list (make-csv-reader row-str csv-spec))
       [(list row) (set-add! input-csv-set row)]
       [_ (error 'csv-set "invalid row: ~v" row-str)]))
   
   (define set-list (set->list input-csv-set)) 
   (define sort-result (*sort-result*))
   
   (define sorted-csv
     (cond
       [sort-result (sort-by-cols set-list sort-result)] 
       [else set-list]))

   (define named-csv
     (cond
       [(*config->name*) 
        =>
        (lambda (c) 
          (parameterize ([name-sep " "] [name-end ""])
            (map-col sorted-csv c (lambda (x) (config->name (s->n x))))))]
       [else sorted-csv]))

   (parameterize ([current-output-port ((*open-output!*))])
     (for ([row (in-list named-csv)])
       (display (string-join row ","))
       (newline))
     (close-output-port (current-output-port)))))
        
   
   
