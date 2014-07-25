#lang typed/racket

(require/typed racket/base
	       [srcloc->string (srcloc . -> . (U False String))])
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


#| 
   Exceptions thrown during parse 
   Note: exceptions are defined as macros so that the 
   stacktraces are more accurate.
|#
(struct exn:schml:parse exn ())

(define-syntax-rule (raise-parse-exn l fmt args ...)
  (raise (exn:schml:parse
	  (format "Exception in parse:~a: ~a" 
		  (srcloc->str l) 
		  (format fmt args ...))
	  (current-continuation-marks))))
	    

(define-syntax-rule (raise-unbound-variable-exn s l)
  (raise-parse-exn l "Unbound variable ~a" s))

(define-syntax-rule (raise-file-empty-exn f)
  (raise-parse-exn (file->srcloc f) "The file is empty"))

(define-syntax-rule (raise-<1-exp-exn f)
  (raise-parse-exn (file->srcloc f) 
		   "The file contains more than one expression"))

(define-syntax-rule (raise-unsupported-stx-exn stx src)
  (raise-parse-exn src 
		   "unsupported syntax in expression ~a" 
		   (syntax->datum stx)))

(define-syntax-rule (bad-syntax-application stx)
  (raise-parse-exn (syntax->srcloc stx)
		   "bad syntax in application ~a"
		   (syntax->datum stx)))

(define-syntax-rule (raise-unsupported-syntax-exn stx)
  (raise-parse-exn (syntax->srcloc stx)
		   "Unsupported syntax in ~a"
		   (syntax->datum stx)))

(define-syntax-rule (raise-blame-label-exn stx)
  (raise-parse-exn (syntax->srcloc stx)
		   "Expected a string for a blame label got ~a"
		   (syntax->datum stx)))

(define-syntax-rule (raise-type-exn stx)
  (raise-parse-exn (syntax->srcloc stx)
		   "Expected a type for got ~a"
		   (syntax->datum stx)))

(define-syntax-rule (raise-duplicate-binding sym src)
  (raise-parse-exn src "duplicate bindings for ~a" sym))

(define-syntax-rule (raise-reservered-sym sym src)
  (raise-parse-exn src "attemp to bind reserved symbol ~a" sym))

(define-syntax-rule (raise-fml-exn stx)
  (raise-parse-exn (syntax->srcloc stx)
		   "bad syntax in formal(s) ~a"
		   (syntax->datum stx)))

(define-syntax-rule (raise-bnd-exn stx)
  (raise-parse-exn (syntax->srcloc stx) 
		   "bad syntax in bnd(s) ~a" 
		   (syntax->datum stx)))

(define-syntax-rule (rebuild-stx* s s*)
  (cons s (map syntax->datum s*)))

(define-syntax-rule (raise-lambda-exn stx* src)
  (raise-parse-exn src "bad syntax in ~a" (rebuild-stx* 'lambda stx*)))

(define-syntax-rule (raise-let-exn form stx* src)
  (raise-parse-exn src "bad syntax in ~a" (rebuild-stx* 'form stx*)))

(define-syntax-rule (raise-if-exn stx* src)
  (raise-parse-exn src "bad syntax in ~a" (rebuild-stx* 'if stx*)))

(define-syntax-rule (raise-ascribe-exn stx* src)
  (raise-parse-exn src "bad syntax in ~a" (rebuild-stx* ': stx*)))


(struct exn:schml:type exn:schml ())
(struct exn:schml:type:static exn:schml:type ())
(struct exn:schml:type:dynamic exn:schml:type ())

(define-syntax-rule (raise-static-type-exn src fmt args ...)
  (raise (exn:schml:type:static
	  (format "Error in Type-Check:~a: ~a"
		  (srcloc->string src)
		  (format fmt args ...))
	  (current-continuation-marks))))

(define-syntax-rule (raise-variable-not-found src id)
  (raise-static-type-exn 
   src
   "ERROR internal to compiler uvar ~a not found in env" id))

(define-syntax-rule (raise-lambda-inconsistent src tb ta)
  (raise-static-type-exn 
   src 
   "Lambda annotated return type ~a is inconsistent with actual return type ~a"
   ta tb))

(define-syntax-rule (raise-binding-inconsistent src id t-bnd t-exp)
  (raise-static-type-exn
   src
   "~a binding in let annotated by ~a is inconsistent with actual type ~a"
   id t-bnd t-exp))


(define-syntax-rule (raise-ascription-inconsistent src label t-exp t-cast)
  (raise-static-type-exn
   src
   (or label 
       (format "Ascription of type ~a is inconsistent with ~a" t-exp t-cast))))


(define-syntax-rule (raise-if-inconsistent-branches src t-csq t-alt)
  (raise-static-type-exn
   src
   "If branches have inconsistent types ~a and ~a"
    t-csq t-alt))

(define-syntax-rule (raise-if-inconsistent-test src tst)
  (raise-static-type-exn
   src
   "If test is of type ~a which is not consistent with Bool"
   tst))


(define-syntax-rule (raise-app-inconsistent src rator rand*)
  (raise-static-type-exn
   src
   "Inconsistent types in application of type ~a to arguments of type(s) ~a"
   rator rand*))

(define-syntax-rule (raise-app-not-function src t-rator)
  (raise-static-type-exn
   src
   "Application of non function type ~a"
   t-rator))

(define-syntax-rule (raise-letrec-restrict src)
  (raise-static-type-exn
   src
   "Letrecs may only bind well annotated lambda's"))

(define-syntax-rule (raise-only-single-arity-fns src)
  (raise-static-type-exn
   src
   "For the time being all function must be of arity 1"))

(define-syntax-rule (raise-only-single-arity-ret src)
  (raise-static-type-exn
   src
   "For the time being all returns must be of arity 1"))

#|
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
