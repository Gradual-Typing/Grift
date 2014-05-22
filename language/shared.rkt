#lang racket

;; Unique Variables
(provide uvar uvar? uvar-prefix uvar-suffix uvar=? split-uvar)

(define unique (box 1))

(define (uvar p)
  (let ((u (string->symbol 
	    (format "~a$~a" p (unbox unique)))))
    (set-box! unique (add1 (unbox unique)))
    u))

(define (uvar? d)
  (let-values (((p n) (split-uvar d)))
    (if (and p (< 0 n (unbox unique))) #t #f)))

(define (uvar-prefix d)
  (let-values (((p s) (split-uvar d)))
    p))

(define (uvar-suffix d)
  (let-values (((p s) (split-uvar d)))
    s))

(define uvar=? eq?)

(define (split-uvar d)
  (if (symbol? d)
      (let* ((w (symbol->string d))
	     (l (string-length w)))
	(let loop ((n 0))
	  (if (>= n l)
	      (values #f 0)
	      (if (and (char=? (string-ref w n) #\$) (< (add1 n) l))
		  (let ((i (string->number (substring w (add1 n) l))))
		    (if i 
			(values (substring w 0 n) i)
			(values #f 0)))
		  (loop (add1 n))))))
      (values #f 0)))









