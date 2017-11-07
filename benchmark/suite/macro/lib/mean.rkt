#lang racket/base

(module+ main
  (define who 'mean.rkt)
  (require racket/cmdline math/statistics racket/sequence)
  (define decimal-places (make-parameter 4))
  (command-line
   ;;#:progam "mean"
   ;;#:usage-help
   ;;"Compute the mean of real numbers from a file or standard in."
   #:once-each
   [("-d" "--decimal-places") num
    ((format "number of decimal places to show (default: ~a)" (decimal-places)))
    (define n (string->number num))
    (unless (and n (exact-integer? n) (<= 0 n))
      (error who "--decimal-places expected integer in ~a" num))
    (decimal-places n)]
   [("-f" "--file") file
    "take input from file instead of standard in"
    (define p (string->path file))
    (unless (and p (file-exists? p))
      (error who "--file expected file got ~a" file))
    (current-input-port (open-input-file p))]
   #:args ()
   (define ns (sequence->list (in-port)))
   (unless (andmap real? ns)
     (error who "expected only real numbers"))
   (define u (mean ns))
   (display (real->decimal-string u (decimal-places)))))
