#lang racket
(require Schml/compiler/read/reader)

(define test-dir "tests")

(define (run-test cc t)
  (match t
    [(? number? n) (test cc n (empty-test-state))]
    ['all     (test-all cc (empty-test-state))]
    ['valid   (test-valid cc (empty-test-state))]
    ['invalid (test-invalid cc (empty-test-state))]
    [(var a) (error 'run-test "Invalid test ~a" a)]))

(define (test-many pred? from to cc state)
  (if (< n to)
      (if (pred? n)
	  (test-many (add1 n) to cc (test cc n s))
	  (test-many (add1 n) to cc s))
      s)))

(define (test-valid cc state)
  (test-many valid? 0 max-test cc state))

(define (test-invalid cc state)
  (test-many invalid? 0 max-test cc state))

(define (test-all cc state)
  (test-invalid cc (test-valid cc state)))

(define (test cc n s)
  (with-handlers [(exn? 
		   (lambda (e)
		     (dict
		     (result (add1 (result-count s))
			     (result-pass s)
			     (add1 (result-fail s))
			     (
		(lambda ()
		  (cc (read-file 
		       (build-path test-dir 
				   (format "~a.test" n)))))))


;; test-state
(struct result (count pass fail dict))

(update
