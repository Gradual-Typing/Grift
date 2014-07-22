#lang typed/racket

(provide (all-defined-out))

(struct exn:schml exn ())
(struct exn:schml:pass exn:schml ())

;; This is a generic error handler that is usefull for
;; quick testing it should not be used for actuall errors
;; once the code is complete.

(define-syntax-rule (raise-pass-exn who fmt args ...)
  (raise 
   (exn:schml:pass 
    (format "Error in ~a: ~a"
	    who
	    (format fmt args ...))
    (current-continuation-marks))))

#| Errors thrown in the read pass |#
(struct: exn:schml:read exn:schml ())

(define file-name-fmt
  "Error in read: unable to extract file name from the path ~a")

(define-syntax-rule (raise-file-name-exn p)
  (raise (exn:schml:read
	  (format file-name-fmt (path->string p))
	  (current-continuation-marks))))

#| Errors thrown in the parse pass 
(struct: exn:schml:Syntax exn:schml ())
(struct: exn:schml:Syntax:unbound exn:schml:Syntax ())
(struct: exn:schml:Syntax:not-supported exn:schml:Syntax ())

(define: (bad-syntax [s : srcloc] [d : Any] [e : E)
  (configure-for-external-error)
  (raise (exn:schml:Syntax 
          (format "~a: Invalid Syntax ~a in ~a" 
                  (srcloc->string src) datum exp)
          (current-continuation-marks)
          src exp)))

(define (unbound src var exp)
  (configure-for-external-error)
  (raise (exn:schml:Syntax:unbound 
          (format "~a: Unbound Identifier ~a in ~a" 
                  (srcloc->string src) var exp)
          (current-continuation-marks)
          src exp)))

(define (stx-not-supported msg src exp)
  (configure-for-external-error)
  (raise (exn:schml:Syntax:not-supported 
          (format "~a: Syntax not supported ~a in ~a" 
                  (srcloc->string src) msg exp)
          (current-continuation-marks)
          src exp)))

(struct exn:schml:Type exn:schml ())
(struct exn:schml:Type:Static exn:schml:Type ())
(struct exn:schml:Type:Dynamic exn:schml:Type ())


(define (lambda/inconsistent-types-error src tb ta)
  (configure-for-external-error)
  (raise (exn:schml:Type:Static
          (format "~a: Lambda annotated return type ~a is inconsistent with actual return type ~a"
                  (srcloc->string src) ta tb)
          (current-continuation-marks))))

(define (let-binding/inconsistent-type-error src id t-bnd t-exp)
  (configure-for-external-error)
  (raise (exn:schml:Type:Static
          (format "~a: ~a binding in let annotated by ~a is inconsistent with actual type ~a"
                  (srcloc->string src) id t-bnd t-exp)
          (current-continuation-marks))))

(define (cast/inconsistent-types-error src label t-exp t-cast)
  (configure-for-external-error)
  (let ((msg (or label (format "~a: Cast between inconsistent types ~a and ~a"
                               (srcloc->string src) t-exp t-cast))))
    (raise (exn:schml:Type:Static msg (current-continuation-marks)))))

(define (if/inconsistent-branches-error src t-csq t-alt)
  (configure-for-external-error)
  (raise (exn:schml:Type:Static
          (format "~a: If branches have inconsistent types ~a and ~a"
                  (srcloc->string src) t-csq t-alt)
          (current-continuation-marks))))

(define (if/inconsistent-test-error src tst)
  (configure-for-external-error)
  (raise (exn:schml:Type:Static
          (format "~a: If test is of type which is not consistent with Bool"
                  (srcloc->string src))
          (current-continuation-marks))))

(define (app-inconsistent-error src rator rand*)
  (configure-for-external-error)
  (let ((line1 (format "~a: Application of function with type ~a\n"
                       (srcloc->string src) rator))
        (line2 (format "to arguments of inconsistent types ~a" rand*)))
    (raise (exn:schml:Type:Static (string-append line1 line2) (current-continuation-marks)))))

(define (app-non-function-error src t-rator)
  (configure-for-external-error)
  (raise (exn:schml:Type:Static
          (format "~a: Application of non function type ~a"
                  (srcloc->string src) t-rator)
          (current-continuation-marks))))

(define raise-dynamic-type-error
  (case-lambda
    [(blame-label)
     (configure-for-external-error)
     (raise (exn:schml:Type:Dynamic blame-label (current-continuation-marks)))]
    [(down up)
     (configure-for-external-error)
     (let ((msg (format "Blame ~a and ~a" down up)))
       (raise (exn:schml:Type:Dynamic msg (current-continuation-marks))))]))
|#
