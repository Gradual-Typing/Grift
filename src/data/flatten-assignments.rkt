#lang typed/racket

(require "../language.rkt")

(: flatten-assignments (Data3-Lang Config -> Data4-Lang))
(define (flatten-assignments prog config)
  (TODO finish flatten-assignments))

#|

(define (Set-Bang var val)
  (let ((Set-Bang-dr (lambda (val^) (Set-Bang var val^))))
    (match val
      [,trv (guard (or (uvar? trv) (int64? trv) (label? trv)))
            `(set! ,var ,trv)]
      [(alloc ,x) `(set! ,var (alloc ,x))]
      [(mref ,x ,y) `(set! ,var (mref ,x ,y))]
      [(if ,[Pred -> test] ,[Set-Bang-dr -> conseq] ,[Set-Bang-dr -> altern])
       `(if ,test ,conseq ,altern)]
      [(begin ,[Effect -> ef*] ... ,[Set-Bang-dr -> val])
       `(begin ,ef* ... ,val)]
      [(,binop ,x ,y)    `(set! ,var (,binop ,x ,y))]
      [(,trv1 ,trv* ...) `(set! ,var (,trv1 ,trv* ...))]
      [,err (error who "unmatched datum in Set-Bang: ~a" err)])))

|#
