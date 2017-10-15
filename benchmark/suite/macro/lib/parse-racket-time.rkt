#lang racket/base
(require racket/match
         racket/port
         racket/function)

;; Print the realtime 
(define (parse-racket-time str [result-rx #f] [type 'real])
  (define time-rx #px"cpu time: (\\d+) real time: (\\d+) gc time: (\\d+)")
  (match (regexp-match time-rx str)
    [(list match cpu real gc)
     (when result-rx
       (error 'parse-racket-time "TODO: implement output verification"))
     (define time
       (case type
         [(real) real]
         [(cpu) cpu]
         [else (error 'parse-racket-time "invalid time type: ~a" type)]))
     (display (exact->inexact (/ (string->number time) 1000)))]
    [#f (error 'parse-racket-time "failed parse with ~a"
               `(regexp-match ,time-rx ,str))]
    [other (error 'parse-racket-time
                  "unexpected value from regexp-match:~a"
                  other)]))

(module+ main
  (require racket/cmdline)
  (define result-rx-opt (make-parameter #f)) 
  (define (fact n)
    (if (<= n 1)
        1
        (* n (fact (- n 1)))))
  (command-line
   #:once-each
   [("-p") pregex-str
    "verify the expected output of benchmark (none)"
    (result-rx-opt (pregexp pregex-str))]
   #:args args
   (match args
     [(list)
      (parse-racket-time
       #;(with-output-to-string
           (thunk (time (fact 10000))))
       (port->string (current-input-port))
       (result-rx-opt))]
     [other (error 'parse-racket-time.rkt "args: ~v" args)])))

