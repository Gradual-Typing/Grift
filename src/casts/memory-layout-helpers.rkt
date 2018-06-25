#lang typed/racket

(require "../language/data0.rkt"
         "../language/syntax.rkt")

(provide (all-defined-out))

(: sr-tagged-array-ref (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-tagged-array-ref e t i) (sr-array-ref (sr-untag e t) i))

(: sr-tagged-array-set! (D0-Expr D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-tagged-array-set! e t i v) (sr-array-set! (sr-untag e t) i v))

(: sr-array-ref (D0-Expr D0-Expr -> D0-Expr))
(define (sr-array-ref e i) (op$ Array-ref e i))

(: sr-array-set! (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-array-set! e i v) (op$ Array-set! e i v))

(: sr-untag (D0-Expr D0-Expr -> D0-Expr))
(define (sr-untag e t) (op$ binary-xor e t))

(: unmask-rest : D0-Expr D0-Expr -> D0-Expr)
(define (unmask-rest e m) (op$ binary-and e (op$ binary-not m)))

(: tagged-array-ref : D0-Expr D0-Expr D0-Expr -> D0-Expr)
(define (tagged-array-ref e tm i)
  (sr-array-ref (unmask-rest e tm) i))

(: check-tag? : D0-Expr D0-Expr -> D0-Expr)
(define (check-tag? e m) (op$ not (op$ = (Quote 0) (sr-get-tag e m))))

(: sr-tag-value (D0-Expr D0-Expr -> D0-Expr))
(define (sr-tag-value e t)
  (match t
    [(Quote 0) e]
    [tag (Op 'binary-or `(,e ,tag))]))

(: sr-check-tag=? (D0-Expr D0-Expr D0-Expr -> D0-Expr))
(define (sr-check-tag=? e mask tag) (op$ = (sr-get-tag e mask) tag))

(: sr-plus (D0-Expr D0-Expr -> D0-Expr))
(define (sr-plus f s) (op$ + f s))

(: sr-get-tag (D0-Expr D0-Expr -> D0-Expr))
(define (sr-get-tag e mask) (op$ binary-and e mask))

;; Allocate without forgetting to lift evaluating subterms first
;; this prevents evaluating terms which may cause more allocation
;; will initializing the values of an allocation
;; it essentially produces expression of the form:
(: sr-alloc (String (Option D0-Expr) (Listof (Pair String D0-Expr)) -> D0-Expr))
(define (sr-alloc name tag? slots)
  ;; As long as this is used to initialize all the data I have
  ;; a faily stong guarentee that no other allocation could possibly occur.
  (: sr-alloc-init ((Var Uid) -> (Index (Var Uid) -> D0-Expr)))
  (define ((sr-alloc-init mem) offset value)
    (op$ Array-set! mem (Quote offset) value))
  (define size (length slots))
  (when (= size 0)
    (error 'specify-representation "Empty objects can not be allocated"))
  (define-values (rev-ass* rev-var*)
    (for/fold ([a* : D0-Expr* '()]
               [v* : (Listof (Var Uid)) '()])
              ([b slots])
      (match-define (cons n e) b)
      (define u (next-uid! n))
      (values (cons (Assign u e) a*) (cons (Var u) v*))))
  (define ass* (reverse rev-ass*))
  (define var* (reverse rev-var*))
  (define ind* (range 0 size))
  (define-track-next-uid!$ alloc-id)
  (define alloc-var (Var alloc-id))
  (define alloc-ass (Assign alloc-id (op$ Alloc (Quote size))))
  (define set* (map (sr-alloc-init alloc-var) ind* var*)) 
  (define tag-return : D0-Expr
    (cond
      [(not tag?) alloc-var]
      [else (sr-tag-value alloc-var tag?)]))
  (Begin (append ass* (cons alloc-ass set*)) tag-return))
