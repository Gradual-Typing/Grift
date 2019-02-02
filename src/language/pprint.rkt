#lang racket/base

(provide
 type->sexp
 type->string)

(require
 racket/match
 racket/port
 "./forms.rkt")


(define (type->sexp t [e (hash)])
  (let rec ([t t])
    (match t 
      [(FVar id) (hash-ref e id)]
      [(Fn n a* r) `(,@(map rec a*) -> ,(rec r))]
      [(Mu s)
       (define u (next-uid! ""))
       (define X (gensym "X"))
       (define t0 (grift-type-instantiate s (FVar u)))
       `(Rec ,X ,(type->sexp t0 (hash-set e u X)))]
      [(STuple n a*) `(Tuple ,@(map rec a*))]
      [(GVect a) `(GVect ,(rec a))]
      [(GRef a) `(GRef ,(rec a))]
      [(MVect a) `(MVect ,(rec a))]
      [(MRef a)  `(MRef ,(rec a))]
      [(Unit) '()]
      [other
       (define-values (nfo _0) (struct-info other))
       (define-values (sym _1 _2 _3 _4 _5 _6 _7) (struct-type-info nfo))
       sym])))

(define (type->string t)
  (with-output-to-string
    (lambda () (display (type->sexp t (hash))))))
