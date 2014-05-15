#lang racket
(require (for-syntax  racket/list))

(provide (all-defined-out))

;; (map/values proc ls '() ...)
;; maps a procedure that returns n values over ls creating n lists
;; returned as values. n must be the same as the number of empty lists
;; provided

(define-syntax map/values
  (lambda (stx)
    (syntax-case stx ()
      [(_ p l v ...)
       (with-syntax ([(t ...) (generate-temporaries #'(v ...))]
                     [(t* ...) (generate-temporaries #'(v ...))])
         #'(let loop ((l* l))
             (if (null? l*)
                 (values v ...)
                 (let-values (((t ...) (p (car l*)))
                              ((t* ...) (loop (cdr l*))))
                   (values (cons t t*) ...)))))])))

(define-syntax vector-map/values
  (lambda (stx)
    (syntax-case stx ()
      [(_ p v n)
       (let ((n (syntax->datum #'n)))
         (unless (and (fixnum? n) (> n -1))
           (raise-syntax-error 'array-map/values
                               "last argument must be a non-negative fixnum"))
         (with-syntax ([(r ...) (generate-temporaries (make-list n 'return-array))]
                       [(t ...) (generate-temporaries (make-list n 'tmp))])
           #'(let* ([proc p] [vec v] [l (vector-length v)]
                    [r (make-vector l)] ...)
               (do ([i 0 (add1 i)])
                   ((= i l) (values r ...))
                 (let-values ([(t ...) (proc (vector-ref vec i))])
                   (vector-set! r i t) ...)))))])))

(define (empty-env) (hasheq))
(define (env-extend env key val) (hash-set env key val))
(define (env-lookup env key failure) 
  (hash-ref env key failure))
(define (env-extend/env f s)
  (for/fold ([h f]) ([(k v) (in-hash s)]) (hash-set h k v))) 

(define (length=? l n)
  (define (do-it l n) (or (and (pair? l) (positive? n) (do-it (cdr l) (sub1 n)))
                          (and (not (pair? l)) (zero? n))))
  (if (and (number? n) (pair? l))
      (do-it l n)
      (error 'length=? "type mismatch")))


