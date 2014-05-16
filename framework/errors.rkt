#lang racket

(provide (all-defined-out))

(struct Schml exn ())
(struct Schml:Pass Schml (pass))
(struct Schml:Pass:Match Schml:Pass ())
(struct Schml:Pass:Cond  Schml:Pass ())

(define (pass-error who fmt . a)
  (configure-for-internal-error)
  (raise (Schml:Pass
          (apply format `(,(format "~~a:~a" fmt) ,who . ,a))  
          (current-continuation-marks)
          who)))

(define (match-pass-error who which thing)
    (configure-for-internal-error)
    (raise (Schml:Pass:Match
	    (format "~a:Match Error at ~a with irritant ~a"
		    who which thing)
	    (current-continuation-marks)
	    who)))

(define (cond-pass-error who which thing)
    (configure-for-internal-error)
    (raise (Schml:Pass:Match
	    (format "~a:Cond Error at ~a with irritant ~a"
		    who which thing)
	    (current-continuation-marks)
	    who)))

(struct Schml:Syntax Schml (location expression))
(struct Schml:Syntax:unbound Schml:Syntax ())
(struct Schml:Syntax:not-supported Schml:Syntax ())


(define-syntax configure-for-external-error
  (syntax-rules ()
    [(_) (begin (error-print-source-location #f)
                (error-print-context-length 0))]))

(define-syntax configure-for-internal-error
  (syntax-rules ()
    [(_) (begin (error-print-source-location #t)
                (error-print-context-length 5))]))

(define (bad-syntax src datum exp)
  (configure-for-external-error)
  (raise (Schml:Syntax 
          (format "~a: Invalid Syntax ~a in ~a" 
                  (srcloc->string src) datum exp)
          (current-continuation-marks)
          src exp)))

(define (unbound src var exp)
  (configure-for-external-error)
  (raise (Schml:Syntax:unbound 
          (format "~a: Unbound Identifier ~a in ~a" 
                  (srcloc->string src) var exp)
          (current-continuation-marks)
          src exp)))

(define (stx-not-supported msg src exp)
  (configure-for-external-error)
  (raise (Schml:Syntax:not-supported 
          (format "~a: Syntax not supported ~a in ~a" 
                  (srcloc->string src) msg exp)
          (current-continuation-marks)
          src exp)))
