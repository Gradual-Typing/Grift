#lang racket
;; Unique Variables
(provide (all-defined-out))
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
	  (if (not (< n l))
	      (values #f 0)
	      (if (char=? (string-ref w n) #\$)
		  (let ((n (string->number (substring w (add1 n) l))))
		    (if n 
			(values (substring w 0 n) n)
			(values #f 0)))
		  (loop (add1 n))))))
      (values #f 0)))

;; Operators
(define (symbolic-primitiveq s)
  (and (symbol? s)
       (case s
         [(%<) (Prim:Rel:Int:<)]
         [(%>) (Prim:Rel:Int:>)]
         [(%=) (Prim:Rel:Int:=)]
         [(%<=) (Prim:Rel:Int:<=)]
         [(%>=) (Prim:Rel:Int:>=)]
         [(%*)   (Prim:Bin:Int:*)]
         [(%+)   (Prim:Bin:Int:+)]
         [(%-)   (Prim:Bin:Int:-)]
         [(%and) (Prim:Bin:Int:and)]
         [(%or) (Prim:Bin:Int:or)]
         [(%>>) (Prim:Bin:Int:>>)]
         [(%<<) (Prim:Bin:Int:<<)])))

(struct Prim ())
(struct Prim:Rel Prim ())
(struct Prim:Rel:Int Prim:Rel ())
(struct Prim:Rel:Int:<  Prim:Rel:Int ())
(struct Prim:Rel:Int:>  Prim:Rel:Int ())
(struct Prim:Rel:Int:=  Prim:Rel:Int ())
(struct Prim:Rel:Int:<= Prim:Rel:Int ())
(struct Prim:Rel:Int:>= Prim:Rel:Int ())
(struct Prim:Bin Prim ())
(struct Prim:Bin:Int Prim:Bin ())
(struct Prim:Bin:Int:*   Prim:Bin:Int ())
(struct Prim:Bin:Int:+   Prim:Bin:Int ())
(struct Prim:Bin:Int:-   Prim:Bin:Int ())
(struct Prim:Bin:Int:and Prim:Bin:Int ())
(struct Prim:Bin:Int:or  Prim:Bin:Int ())
(struct Prim:Bin:Int:>>  Prim:Bin:Int ())
(struct Prim:Bin:Int:<<  Prim:Bin:Int ())






