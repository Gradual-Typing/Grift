#lang racket/base
(require
 racket/function
 racket/file
 racket/match
 racket/port
 racket/string)


(define racket-time-rx #px"cpu time: \\d+ real time: (\\d+) gc time: \\d+")

(define (racket-time->seconds time)
  (define n (string->number time))
  (unless (exact-integer? n)
    (error 'racket-time->seconds "expected exact integer: ~a" time))
  (/ (exact->inexact n) 1000))

(define grift-time-rx #px"time \\(sec\\): (\\d+\\.\\d+)\n")

(define ocaml-time-rx grift-time-rx)

(define chez-time-rx
  (pregexp 
   #<<eof
\(time .+\)
\s+\w+ collections?
\s+\d+\.\d+s elapsed cpu time.*
\s+(\d+\.\d+)s elapsed real time.*
\s+\d+ bytes allocated.*
eof
   ))


(define gambit-time-rx 
  (pregexp
   #<<eos
\(time .+\)
\s+(\d+) ms real time
\s+\d+ ms cpu time
eos
   ))

(define (exact-millisecond-str-time->seconds time)
  (define n (string->number time))
  (unless (exact-integer? n)
    (error 'exact-millisecond-str-time->seconds
           "expected exact integer: ~a" time))
  (/ (exact->inexact n) 1000))

(define ((parse-time time-rx time->seconds) result-str [expect-str ""])
  (unless (string-contains? result-str expect-str)
    (error 'parse-time "expected=\n~a\ngot=\n~a\n"
           expect-str result-str))
  (match result-str
    [(regexp time-rx (list _ time)) (time->seconds time)]
    [other
     (error 'parse-time "Failed to parse time:\n~v" result-str)]))

(define ((strip-time time-rx) result-str)
  ;; Assumes the usable result comes just before the time
  (match (regexp-match-positions time-rx result-str)
    [(list (cons start-time  _) _ ...)
     (string-trim (substring result-str 0 start-time))]
    [_ (error 'strip-time "time not found in:\n ~v" result-str)]))

      

(define parse-racket-time
  (parse-time racket-time-rx exact-millisecond-str-time->seconds))
(define strip-racket-time (strip-time racket-time-rx))

(define parse-gambit-time
  (parse-time gambit-time-rx exact-millisecond-str-time->seconds))
(define strip-gambit-time (strip-time gambit-time-rx))

(define parse-chez-time (parse-time chez-time-rx string->number))
(define strip-chez-time (strip-time chez-time-rx))

(define parse-grift-time (parse-time grift-time-rx string->number))
(define strip-grift-time (strip-time grift-time-rx))

(define parse-ocaml-time (parse-time ocaml-time-rx string->number))
(define strip-ocaml-time (strip-time ocaml-time-rx))

(module+ main
  (require racket/cmdline)
  (define expected-result (make-parameter ""))
  (define (err-select-lang . a)
    (error 'parse-time.rkt "select a language"))
  (define lang-parse-time (make-parameter err-select-lang))
  (define lang-strip-time (make-parameter err-select-lang))
  (define (parse-time-program)
    (display
     ((lang-parse-time)
      (port->string (current-input-port))
      (expected-result))))
  (define (strip-time-program)
    (display ((lang-strip-time) (port->string (current-input-port)))))
  (define main (make-parameter parse-time-program))
  (define lang-hash
    (make-hash
     (list (cons "racket" (cons parse-racket-time strip-racket-time))
           (cons "typed_racket" (cons parse-racket-time strip-racket-time))
           (cons "typed-racket" (cons parse-racket-time strip-racket-time))
           (cons "grift" (cons parse-grift-time strip-grift-time))
           (cons "dyn" (cons parse-grift-time strip-grift-time))
           (cons "static" (cons parse-grift-time strip-grift-time))
           (cons "gambit" (cons parse-gambit-time strip-gambit-time))
           (cons "ocaml" (cons parse-ocaml-time strip-ocaml-time))
           (cons "chezscheme" (cons parse-chez-time strip-chez-time)))))
  (command-line
   #:once-each
   ["--lang" lang
    ((format "select a language: ~a" (string-append* (hash-keys lang-hash))))
    (define (err) (error '--lang "invalid: ~a" lang)) 
    (match-define (cons parse strip) (hash-ref lang-hash lang err))
    (lang-parse-time parse)
    (lang-strip-time strip)]
   [("--strip")
    "remove language specific timing info from input"
    (main strip-time-program)]
   [("--expect") file
    "File containing the expected input to verify result"
    (define p (string->path file))
    (unless (and p (file-exists? p))
      (error '--expect "file not found ~a" file))
    (expected-result (file->string p))]
   [("--in") f
    "File input"
    (current-input-port
     (case f
       [("3") (current-input-port)]
       ;; copy the file to string in case the file has
       ;; been specified as output also
       [else  (open-input-string (file->string f))]))]
   [("--out") f
    "File output"
    (current-output-port
     (case f
       [("1") (current-output-port)]
       [("2") (current-error-port)]
       [else (open-output-file f #:exists 'replace)]))]
   #:args () ((main))))

