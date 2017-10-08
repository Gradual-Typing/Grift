#lang racket
(require racket/cmdline math/base)

(define (print-qs-input [order 'random]
                        [count 1000]
                        [port  (current-output-port)])
  (define input
    (let ([l (sequence->list (in-range 0 count))])
      (case order
        [(random) (shuffle l)]
        [(descending) (reverse l)]
        [(ascending)  l])))
  (fprintf port "~a\n" count)
  (for ([e (in-list input)])
    (fprintf port "~a\n" e)))

(module+ main
  (define port  (make-parameter #f))
  (define order (make-parameter 'random))
  (define count (make-parameter 1000))
  (define (make-filename)
    (define order-str
      (case (order)
        [(random) "rand"]
        [(descending) "descend"]
        [(ascending) "ascend"]
        [else (error 'make-filename "invalid order: ~a" (order))]))
    (format "in_~a~a.txt" order-str (count)))
  (command-line
   #:once-each
   [("-o") output-file
    ((format "name of file to save input (~a)" (make-filename)))
    (match output-file
      ["1" (port (current-output-port))]
      ["2" (port (current-error-port))]
      [otherwise
       (with-handlers ()
         (port (open-output-file
                output-file
                #:mode 'text
                #:exists 'replace)))])]
   [("-c") n
    ((format "number of elements in input array (~a)" (count)))
    (define m (string->number n))
    (unless (exact-nonnegative-integer? m)
      (error 'gen.rkt "expected exact nonnegative integer for -c flag"))
    (count m)]
   #:once-any
   [("--descending")
    "sort input array in descending order"
    (order 'descending)]
   [("--ascending")
    "sort input array in ascending order"
    (order 'ascending)]
   
   #:args ()
   (unless (port)
     (port (open-output-file
            (make-filename)
            #:exists 'replace
            #:mode 'text)))
   (print-qs-input (order) (count) (port))))
