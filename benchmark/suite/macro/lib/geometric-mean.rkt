#lang racket/base
(require racket/file
         racket/function
         racket/match
         racket/sequence
         racket/string
         "./csv.rkt")

;; calculates the geometric mean of a non-empty list or vector
(define (geometric-mean ns)
  (cond
    [(and (list? ns) (not (null? ns)))
     (define P (foldl * 1 ns))
     (define c (length ns))
     (expt P (/ 1 c))]
    [(and (vector ns) (not (zero? (vector-length ns))))
     (define P (sequence-fold * 1 ns))
     (define c (vector-length ns))
     (expt P (/ 1 c))]
    [(or (list? ns) (vector? ns))
     (error 'geometric-mean "undefined for zero length collection")]
    [else (raise-argument-error 'geometric-mean "list or vector" 1 ns)]))

(module+ test
  (require rackunit)
  (check-equal? (geometric-mean '(2 18)) 6)
  (check-equal? (geometric-mean #(2 18)) 6)
  (define (check-d=? a e d)
    (define (delta=? a) (<= (- e d) a (+ e d)))
    (check-pred delta=? a (format "not within ~a +/- ~a" e d)))
  (check-d=? (geometric-mean '(10 51.2 8)) 16 #i2e-15)
  (check-d=? (geometric-mean '(1 3 9 27 81)) 9 #i2e-15))

(define (gm-csv csv)
  (match csv
    [(list (list cname* ...) (list rname n** ...) ...)
     (define num-col (length (car n**)))
     (define num-rows (length n**))
     (define (length=nc? l) (= num-col (length l)))
     (define (all-number-str? l)
       (and (andmap string? l)
            (andmap string->number l)))
     (unless (and (andmap length=nc? n**)
                  (andmap all-number-str? n**))
       (error 'gm-csv "invalid data ~v" csv))
     (define gm*
       (for/list ([i (in-range num-col)])
         (define slice (for/list ([c (in-list n**)]) (list-ref c i)))
         (define gm (geometric-mean (map string->number slice))) 
         (number->string (exact->inexact gm))))
     `(,cname* ("geometric mean" . ,gm*) ,@(map cons rname n**))]))


(module+ main
  (require racket/cmdline)
  (define who 'geometric-mean)
  (command-line
   #:args (filename)
   (define p (string->path filename))
   (unless p
     (error who "expected path: ~v" filename))
   (unless (file-exists? p)
     (error who "file doesn't exist: ~a" p))
   (when (directory-exists? p)
     (error who "found directory expected csv: ~a" p))
   (define c (file->csv p))
   (define gm (gm-csv c))
   (display (csv->string gm))))
