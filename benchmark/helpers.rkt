#lang typed/racket

(require "../src/compile.rkt")

(provide (all-defined-out))

;; format helpers for dealing with the directory format that I have been using

;; src must be kept in seperate directories because of gambit's habit of
;; deleting similarly named c files.
(define (src x y)
  (format "src/~a/~a" x y))

(define (bin x y)
  (format "tmp/bin/~a/~a" x y))

(define (itmdt-c x)
  (format "tmp/c/~a.c" x))

(define (itmdt-a x)
  (format "tmp/asm/~a.s" x))

;; Compile a c program in the benchmark directory
(define (c-compile [f : String])
  (system (format "cc ~a -O3    -o ~a" (src 'c f) (bin 'c f)))
  (system (format "cc ~a -O3 -S -o ~a" (src 'c f) (itmdt-a f))))

;; Compile a schml program in the benchmark directory
(define (schml-compile [f : String])
  (compile (src 'schml f)
           #:output (build-path (bin 'schml f))
           #:keep-c (build-path (itmdt-c f))
           #:keep-a (build-path (itmdt-a f))))

;; Compile a gambit scheme program in the benchmark directory
(define (gambit-compile [f : String])
  (let* ([call "gsc "]
         [out  " -o ~a "]
         [opt1  " -prelude \"(declare (standard-bindings)) (declare (block))\" "]
         [opt2  " -cc-options -O3 "]
         [exe   " -exe "]
         [fmt   (string-append call opt1 opt2 exe out " ~a ")])
    (system (format fmt (bin 'gambit f) (src 'gambit f)))))

(: run (->* (Any Any) #:rest Any Boolean))
;; run the given program with the given arguments
(define (run type f . a*)
  ;; Concatenate arguments into one string
  (define (args->string [a* : (Listof Any)])
    (with-output-to-string
      (lambda ()
        (for ([a a*]) (printf " ~a " a)))))
  (system (string-append (bin type f) " " (args->string a*))))

(: discard-value (Any -> Void))
(define (discard-value a)
  (void))



(define (parse-basic-time [tag : Symbol] [num : Natural] [out : String])
  (let ([m* (or (regexp-match rx  out) '())])
   (if (and (not (null? m*)) (car m*))
       (display `(,tag ,num ,(car m*)))
       (error 'parse-b-time "~a ~a: failed to parse output:\n~a" tag num out))))

;; This pattern 
(define basic-time-regexp #px"\\d+.\\d+")

(define gambit-time-regexp
  #| this pattern is meant to match the following output:
  (time (let loop ((i iters)) (if (fx= 0 i) '() (loop (fx- i 1)))))
    29 ms real time
    29 ms cpu time (28 user, 0 system)
    no collections
    no bytes allocated
    4 minor faults
    no major faults
  |#
  (pregexp
   (concat-string-literal
    ".+\n"
    "(\\d+) ms real time\n"
    "\\d+ ms cpu time \\(\\d+\\ user, \\d+ system\\)\n"
    ".+\n"
    ".+\n"
    ".+\n"
    ".+\n")))

(define (parse-gambit-time [num : Natural] [out : String])
  (let ([ptrn  ((regexp-match #px"\\d+.\\d+" out) '())])
   (if (and (not (null? m*)) (car m*))
       (display `(,tag ,num ,(car m*)))
       (error 'parse-b-time "~a ~a: failed to parse output:\n~a" tag num out))))
