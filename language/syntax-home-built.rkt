#lang racket

(struct File (name stx))

(define mark? number?)
(define mark=? eq?)
(define top-mark 0)
(define next-mark (box 1))
(define (make-mark)
  (let ((m (unbox next-mark)))
    (set-box! next-mark (add1 m))
    m))
(define (top-marked? m*) (set-member? m* 0))
(define (marks=? n m)
  (cond
   [(null? n) (null? m)]
   [(null? m) #f]
   [(and (mark=? (car n) (car m))
	 (marks=? (cdr n) (cdr m)))]))



(struct Subst (id marks label))
(struct Stx (expr wrap src))

(define (add-mark x m)
  (extend-wrap x `(,m)))

(define (add-subst x id label)
  (extend-wrap x `(,(Subst (Stx-expr id) (Stx-marks id) label))))

(define (join-wraps w1 w2)
  (cond 
   [(null? w1) w2]
   [(null? w2) w1]
   [else 
    (let ((f (car w2)))
      (let loop ([a (car w1)] [d (cdr w2)])
	(cond
	 [(not (null? d)) (cons a (f (car d) (cdr d)))]
	 [(and (mark? a) (mark=? a f)) (cdr w2)]
	 [else (cons a w2)])))]))

(define (extend-wrap x w)
  (if (Stx? x)
      (Stx (Stx-expr x)
	   ;; the use of join marks supports mark canceling
	   (join-wraps w (Stx-wrap x))
	   (Stx-src x))
      (Stx x w (Src-Loc #f #f #f #f))))

(define (wrap-marks w)
  (if (null? w)
      '()
      (let ((a (car w)))
	(if (mark? a)
	    (cons a (wrap-marks (cdr w)))
	    (wrap-marks (cdr w))))))

(define (Stx-marks x)
  (wrap-marks (Stx-wrap x)))

(define new-var
  (lambda (p)
    (unique-var (Stx-expr p))))

(define strip
  (lambda (s)
    (match s
      [(Stx e m _ l) (if (top-marked? m) e (strip e))]
      [(cons a d) (let ((a^ (strip a)) (d^ (strip d)))
		    (if (and (eq? a a^) (eq? d d^))
			s (cons a^ d^)))]
      [x x])))

(define syntax-error
  (lambda (obj msg)
    (error 'expand "~a ~s" msg (strip obj))))

(define (identifier? x)
  (and (Stx? x)
       (let ((x (Stx-expr x)))
	 (or (symbol? x)
	     (uvar? x)))))

(define (self-evaluating? x)
  (or (boolean? x) (number? x) (string? x) (char? x)))
