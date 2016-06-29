#!/u/dalmahal/bin/bin/gsi-script -:d0

(define (main arg)
  (letrec ([f (lambda (x) x)]
	   [g (lambda (x) 42)])
    (let ([iters (string->number arg)]
	  [acc (box 0)])
      (let ([r (box (if (> iters 0) f g))])
	(let ([id (unbox r)])
	  (letrec ([run-test (lambda (i acc) (id acc))])
	    (time
	     (let loop ([i iters])
	       (cond
		[(zero? i) (unbox acc)]
		[else (set-box! acc (run-test i (unbox acc))) (loop (- i 1))]))
	     (current-output-port))))))))

;; (define (main arg)
;;   (let ([iters (string->number arg)])
;;     (letrec ([f (lambda (x)
;; 		  (if (= x 0)
;; 		      0
;; 		      (+ 0 (f (- x 1)))))]
;; 	     [g (lambda (x) 42)])
;;       (let ([r (box (if (> iters 0) f g))])
;; 	(time
;; 	 ((unbox r) iters)
;; 	 (current-output-port))))))
