#lang racket/base
(provide (all-defined-out))
(require (only-in "helpers.rkt" traces current-log-port)
         (for-syntax racket/base))
#|
The true draw back of using typed racket is that macros typed
macros will not work in untyped code. This module duplicates
any untyped macros so that they will work in untyped code.
I have not yet checked to see if no-check is sufficient to
get around this problem.
|#



;; Untyped macros
(define-syntax-rule (todo x ...)
  (error 'todo (display '(todo x ...))))

;; Type annotations are no-ops 
(define-syntax-rule (: any ...) (void))
(define-syntax-rule (define-type any ...) (void))


(define-syntax-rule (trace? s ...)
  (let ([t*? (traces)])
    (or (memq s t*?) ...)))

(define-syntax-rule (logf fmt a ...)
  (let ()
    (fprintf (current-log-port) fmt a ...)
    (flush-output (current-log-port))))

(define-syntax-rule (pass/log (o ...) (p in c ...))
  (let ([t? (trace? 'p 'All 'o ...)])
    (when t? (logf "~v input:\n~v\n\n" 'p in))
    (let ([out (p in c ...)])
      (when t? (logf "~v output:\n~v\n\n" 'p in))
      out)))

(define-syntax logging
  (syntax-rules ()
    [(logging n () f a ...) (logging n (All) f a ...)]
    [(logging n (o0 o* ...) f a ...)
     (let ([t? (trace? 'n 'o0 'o* ...)])
       (when t? (logf (format "~a: ~a\n\n" 'n f) a ...)))]))

(define-syntax-rule (log-body n (v ...) e ... b)
  (let ([t? (trace? 'n 'All)])
    (when t?
      (logf "~v input:\n" 'n)
      (logf "\t~v : ~v\n" 'v v) ...
      (logf "\n"))
    e ...
    (let ([out b])
      (when t?
        (logf "~v output:\n\t~v\n\n" 'n out)
        (flush-output (current-log-port)))
      out)))

(define-syntax-rule (tracef (s ...) fmt a ...)
  (when (trace? s ...)
    (logf fmt a ...)))

(define-syntax trace-define
  (syntax-rules (->)
    [(_ (f i ...) -> (r ...) e ... b)
     (begin
       (define n (box 0))
       (define (f i ...)
         (define t? (trace? 'All 'f))
         (set-box! n (+ (unbox n) 1))
         (when t?
           (logf "~a@~a:~a=~v\n" 'f (unbox n) 'i i) ...
           (logf "\n")
           (flush-output (current-log-port)))
         e ...
         (let-values ([(r ...) b])
           (set-box! n (- (unbox n) 1))
           (when t?
             (logf "~a@~a:~a=~v\n" 'f (unbox n) 'r r) ...
             (logf "\n")
             (flush-output (current-log-port)))
           (values r ...))))]
    [(_ (f i ...) e ... b)
     (trace-define (f i ...) -> (out) e ... b)]))


(define-for-syntax under-construction?
  (and (getenv "griftUnderConstruction") #t))

(define-for-syntax syntax-id
  (syntax-rules ()
    [(_ x ...) (begin x ...)]))

(define-for-syntax syntax-void
  (syntax-rules ()
    [(_ x ...) (void)]))

;; only run this code if we are working on grift
(define-syntax if-in-construction
  (if under-construction?
      syntax-id
      syntax-void))

(struct exn:todo exn ())
(begin-for-syntax (struct exn:todo exn ()))

(define-syntax TODO
  (if under-construction?
      (syntax-rules ()
        [(_ x ...)
         (raise
          (exn:todo
           (format "TODO: ~a" '(x ...))
           (current-continuation-marks)))])
      (lambda (x)
        (define loc-string
          (srcloc->string
           (srcloc (syntax-source x) (syntax-line x) (syntax-column x)
                   (syntax-position x) (syntax-span x))))
        (raise-syntax-error
         'Unfinished-TODO
         (format "~a: ~a" loc-string (syntax->datum x))))))


(define-syntax-rule (debug v ...)
  (begin (printf "~a=~v\n" 'v v) ... (newline)))
