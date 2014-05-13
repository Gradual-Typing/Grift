#lang racket
(require Schml/language/syntax.rkt)
;; Label : Number
;; Mark : Number






(define (extend-env label binding env)
  `((,label . ,binding) . ,env))

(define (id-binding id env)
  (label-binding id (id-label id) env))

(define (label-binding id label env)
  (let ((a (assq label env)))
    (if a (cdr a) (syntax-error id "displaced lexical"))))

(define (id-label id)
  (let ([i (Stx-expr id)]
	[wrap (Stx-wrap id)])
    (let loop ([wrap wrap] [marks (wrap-marks wrap)])
      (if (null? wrap)
	  (syntax-error id "undefined identifier")
	  (let ((w (car wrap)))
	    (cond
	     [(mark? w) (loop (cdr wrap) (cdr marks))]
	     [(and (ident=? (Subst-id w) i)
		   (marks=? (Subst-marks w) marks))
	      (Subst-label w)]
	     [else (loop (cdr wrap) marks)]))))))

(define (expand stx env meta-env)
  (define (exp-macro tranformer stx)
    (let ([m (make-mark)])
      (add-mark m (trasformer (add-mark m stx)))))
  (define (exp-core transformer stx env meta-env)
    (transformer stx env meta-env))
  (define (exp-exprs stx* env meta-env)
    (map (lambda (x) (exp e env meta-env)) stx*))
  (if (and (not (Stx-pair? stx)))
      (if (identifier? stx)
	  (let ([b (id-binding stx env)])
	    (case (binding-type b)
	      [(macro) (expand (exp-macro (binding-value b) stx) env meta-env)]
	      [(lexical) (binding-val b)]
	      [else (syntax-error stx "invalid syntax")]))
	  (let ([datum (strip stx)])
	    (if (self-evaluating? d)
		d
		(syntax-error stx "invalid syntax")))) 
      (let ((stx-a (Stx-car stx))
	    (stx-d (Stx-car stx)))
	(if (identifier? stx)
	    (let ([b (id-binding stx-a env)])
	      (case (binding-type b)
		[(macro) (expand (exp-macro (binding-value b) stx) env meta-env)]
		[(lexical) (App (Binding-value b) (exp-exprs stx-d env meta-env) (Stx-src stx))]
		[(core) (exp-core (Binding-value b) stx env meta-env)]
		[else (syntax-error stx "invalid syntax")]))
	    (App (expand stx-a env meta-env)
		 (expand-exprs stx-d env meta-env)
		 (Stx-src stx))))))


;; Core form transformers 
(define (exp-quote stx env meta-env)
  (if (Stx-pair? stx)
      (let ((stx-a (Stx-car stx))
	    (stx-d (Stx-cdr stx)))
	(if (Stx-pair? stx-d)
	    (let ((stx-da (Stx-car stx-d))
		  (stx-dd (Stx-cdr stx-d)))
	      (if (Stx-null?)
		  (Quote (strip stx-da) (Stx-src stx))
		  (syntax-error stx "Invalid Syntax")))
	    (syntax-error stx "Invalid Syntax")))
      (error 'exp-quote "we shouldn't be here: ~a" stx)))

(define (exp-if stx env meta-env)
  (if (Stx-length=? 4 stx)
      (let* ((_ (Stx-car stx))
	     (stx^ (Stx-cdr stx))
	     (stx-tst (Stx-car stx^))
	     (stx^ (Stx-cdr stx^))
	     (stx-csq (Stx-car stx^))
	     (stx^ (Stx-cdr stx^))
	     (stx-alt (Stx-car stx^))
	     (stx^ (Stx-cdr stx^)))
	(if (Stx-null? stx^)
	    (If (exp stx-tst env meta-env)
		(exp stx-csq env meta-env)
		(exp stx-alt env meta-env)
		(Stx-src stx))
	    (syntax-error stx "Invalid syntax")))
      (syntax-error stx "Invalid syntax")))

(define (exp-lambda stx env meta-env)
  (if (Stx-length=? 3 stx)
      (let* ((_ (Stx-car stx))
	     (stx^ (Stx-cdr stx))
	     (stx-fml* (Stx-car stx^))
	     (stx^ (Stx-cdr stx^))
	     (stx-body (Stx-car stx^)))
	(let-values  ([(l* v*) (gen-label/var* stx-fml*)])
	  (Lambda v* 
		  (exp (add-subst* stx-fml* l* stx-body) 
		       (extend-env* l* v*)
		       meta-env)
		  (Stx-src stx))))
      (syntax-error stx "Invalid syntax")))







