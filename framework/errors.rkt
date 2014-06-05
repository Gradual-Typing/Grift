#lang racket

(provide (all-defined-out))


(define-syntax configure-for-external-error
  (syntax-rules ()
    [(_) (begin (error-print-source-location #f)
                (error-print-context-length 0))]))

(define-syntax configure-for-internal-error
  (syntax-rules ()
    [(_) (begin (error-print-source-location #t)
                (error-print-context-length 5))]))

(struct Schml exn ())
(struct Schml:Pass Schml ())
(struct Schml:Pass:Match Schml:Pass ())
(struct Schml:Pass:Cond  Schml:Pass ())

(define (pass-error who fmt . a)
  (configure-for-internal-error)
  (raise (Schml:Pass
          (apply format `(,(format "~~a:~a" fmt) ,who ,@a))  
          (current-continuation-marks))))

(define (match-pass-error who which thing)
    (configure-for-internal-error)
    (raise (Schml:Pass:Match
	    (format "~a:Match Error at ~a with irritant ~a"
		    who which thing)
	    (current-continuation-marks))))

(define (cond-pass-error who which thing)
    (configure-for-internal-error)
    (raise (Schml:Pass:Match
	    (format "~a:Cond Error at ~a with irritant ~a"
		    who which thing)
	    (current-continuation-marks))))

(struct Schml:Syntax Schml ())
(struct Schml:Syntax:unbound Schml:Syntax ())
(struct Schml:Syntax:not-supported Schml:Syntax ())

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

(struct Schml:Type Schml ())

(define (lambda/inconsistent-types-error src tb ta)
  (configure-for-external-error)
  (raise (Schml:Type
          (format "~a: Lambda annotated return type ~a is inconsistent with actual return type ~a"
                  (srcloc->string src) ta tb)
          (current-continuation-marks))))

(define (let-binding/inconsistent-type-error src id t-bnd t-exp)
  (configure-for-external-error)
  (raise (Schml:Type
          (format "~a: ~a binding in let annotated by ~a is inconsistent with actual type ~a"
                  (srcloc->string src) id t-bnd t-exp)
          (current-continuation-marks))))

(define (cast/inconsistent-types-error location t-exp t-cast)
  (configure-for-external-error)
  (raise (Schml:Type
          (format "~a: Cast between inconsistent types ~a and ~a"
                  location t-exp t-cast)
          (current-continuation-marks))))

(define (if/inconsistent-branches-error src t-csq t-alt)
  (configure-for-external-error)
  (raise (Schml:Type
          (format "~a: If branches have inconsistent types ~a and ~a"
                  (srcloc->string src) t-csq t-alt)
          (current-continuation-marks))))

(define (if/inconsistent-test-error src tst)
  (configure-for-external-error)
  (raise (Schml:Type
          (format "~a: If test is of type which is not consistent with Bool"
                  (srcloc->string src))
          (current-continuation-marks))))

(define (app-inconsistent-error src rator rand*)
  (configure-for-external-error)
  (let ((line1 (format "~a: Application of function with type ~a\n"
                       (srcloc->string src) rator))
        (line2 (format "to arguments of inconsistent types ~a" rand*)))
    (raise (Schml:Type (string-append line1 line2) (current-continuation-marks)))))

(define (app-non-function-error src t-rator)
  (configure-for-external-error)
  (raise (Schml:Type
          (format "~a: Application of non function type ~a"
                  (srcloc->string src) t-rator)
          (current-continuation-marks))))
