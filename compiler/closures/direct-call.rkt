#lang racket

(define-pass (optimize-direct-call ast conf)
  
  (define (odc-prog prog)
    (match prog
      [(Prog n (app odc-expr e)) (Prog n e)]
      [o (match-pass-error pass 'odc-prog o)]))

  (define (odc-expr exp)
    (match exp
      [(App sa ta (Lambda sl tl (list (Fml i* t*) ...) body) (list e* ...))
       ]))
  
  (if (odc? conf) (odc-prog ast) ast))
