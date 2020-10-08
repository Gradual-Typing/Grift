#lang racket/base
(require
 racket/function
 racket/match
 racket/set
 racket/struct
 "forms.rkt"
 "../helpers-untyped.rkt")

(provide forms=? form=? check-forms=?)

(define ((flip f) . a) (apply f (reverse a)))

(define (forms=? x y [x-env (hash)] [y-env (hash)])
  (define/match (bnd-lhs x)
    [((Bnd x _ _)) x]
    [((list x _)) x]
    [((cons x _)) x])
  (define/match (fml-id x)
    [((Fml x _)) x]
    [((? symbol? x)) x]
    [((? Uid? x)) x])
  (define ((bnd=? x-env y-env) x y)
    (match* (x y)
      [((Bnd _ xt x) (Bnd _ yt y))
       (and (forms=? xt yt x-env y-env)
            (forms=? x y x-env y-env))]
      [((list _ x) (list _ y)) (forms=? x y x-env y-env)]
      [((cons _ x) (cons _ y)) (forms=? x y x-env y-env)]))
  (define ((fml=? x-env y-env) x y)
    (match* (x y)
      [((Fml _ x-ty) (Fml _ y-ty)) (forms=? x-ty y-ty x-env y-env)]
      [((? symbol?) (? symbol?)) #t]
      [((? Uid?) (? Uid?)) #t]
      [(_ _) #f]))
  (define (recur/env x y) (forms=? x y x-env y-env))
  (define res
    (or (eq? x y)
        (match* (x y)
          [((Uid s _) (Uid s _)) #t]
          [((Uid s _) (? symbol? x)) (string=? s (symbol->string x))]
          [((? symbol? x) (Uid s _)) (string=? s (symbol->string x))]
          [((Var x) (Var y))
           ;; NOTE: two unbound variables are considered form=?
           (eq? (hash-ref x-env x #f) (hash-ref y-env y #f))]
          [((Quote x) (Quote y)) (equal? x y)]
          [((Ann x x-ann) (Ann y y-ann))
           (and (or (and (srcloc? x-ann) (srcloc? y-ann))
                    (and (pair? x-ann) (pair? y-ann))
                    (recur/env x-ann y-ann))
                (recur/env x y))]
          [((Let (list xb* ...) x) (Let (list yb* ...) y))
           (and (= (length xb*) (length yb*))
                (andmap (bnd=? x-env y-env) xb* yb*)
                (let* ([gs    (map (thunk* (gensym)) xb*)]
                       [x-env (foldr (flip hash-set) x-env gs (map bnd-lhs xb*))]
                       [y-env (foldr (flip hash-set) y-env gs (map bnd-lhs yb*))])
                  (forms=? x y x-env y-env)))]
          [((Letrec (list xb* ...) x) (Letrec (list yb* ...) y))
           (and (= (length xb*) (length yb*))
                (let* ([gs    (map (thunk* (gensym)) xb*)]
                       [x-env (foldr (flip hash-set) x-env gs (map bnd-lhs xb*))]
                       [y-env (foldr (flip hash-set) y-env gs (map bnd-lhs yb*))])
                  (and (andmap (bnd=? x-env y-env) xb* yb*)
                       (forms=? x y x-env y-env))))]
          [((Lambda (list xf* ...) x) (Lambda (list yf* ...) y))
           (and (= (length xf*) (length yf*))
                (andmap (fml=? x-env y-env) xf* yf*)
                (let* ([gs (map (thunk* (gensym)) xf*)]
                       [x-env (foldr (flip hash-set) x-env gs (map fml-id xf*))]
                       [y-env (foldr (flip hash-set) y-env gs (map fml-id yf*))])
                  (forms=? x y x-env y-env)))]
          [((App x (list xp* ...)) (App y (list yp* ...)))
           (and (recur/env x y) (andmap recur/env xp* yp*))]
          [((Gvector xl xi) (Gvector yl yi))
           (and (recur/env xl yl)
                (recur/env xi yi))]
          [((Switch x (list xc ...) xd) (Switch y (list yc ...) yd))
           (and (forms=? x y x-env y-env)
                (= (length xc) (length yc))
                (for/and ([x (in-list xc)] [y (in-list yc)])
                  (match* (x y)
                    [((cons (list xc ...) x) (cons (list yc ...) y))
                     (and (set=? xc yc) (forms=? x y x-env y-env))]
                    [(_ _) #f]))
                (forms=? xd yd x-env y-env))]
          [((Ascribe x xt xl) (Ascribe y yt yl))
           (and (recur/env x y)
                (recur/env xt yt)
                (equal? xl yl))]
          [((Create-tuple (list xs ...)) (Create-tuple (list ys ...)))
           (and (= (length xs) (length ys)) (andmap recur/env xs ys))]
          [((Tuple-proj x xi) (Tuple-proj y yi))
           (and (equal? xi yi)
                (recur/env x y))] 
          [((list xs ...) (list ys ...))
           (and (= (length xs) (length ys))
                (andmap recur/env xs ys))]
          [((? form? x) (? form? y))
           (define-values (x-st _1) (struct-info x))
           (define-values (y-st _2) (struct-info y))
           (and
            (eq? x-st y-st)
            (andmap recur/env (struct->list x) (struct->list y)))]
          [(x y) (equal? x y)])))
  ;;(debug x y res)
  res)

(define ((form=? x) y)
  (forms=? x y))

(require rackunit)
(define-simple-check (check-forms=? actual expected)
  (forms=? actual expected))

(module+ test
  (check-pred (form=? (Var 'y))
              (Var 'x))
  (check-forms=? (Var 'y) (Var 'x))
  (check-pred (form=? (Let `([x ,(Var 'y)]) (Var 'x)))
              (Let `([y ,(Var 'x)]) (Var 'y)))
  (check-forms=? (Let `([x ,(Var 'y)]) (Var 'x))
                 (Let `([y ,(Var 'x)]) (Var 'y)))
  (check-forms=? (Lambda `(x ,(Fml 'y (Int))) (Var 'y))
                 (Lambda `(y ,(Fml 'x (Int))) (Var 'x)))
  (check-forms=? (Switch (Quote 2) `([(1 2 3) (Quote #t)]) (Quote #f))
                 (Switch (Quote 2) `([(3 2 1) (Quote #t)]) (Quote #f)))
  (check-forms=? (Let `((x . ,(Var 'y))) (Var 'x))
                 (Let `((,(Uid "" 0) . ,(Var 'y))) (Var (Uid "" 0))))

  (define (example-foo x y)
    (Let `((,x . (Quote 1))) (Let `((,y . ,(Var x))) (Var y))))
  (check-forms=? (example-foo (Uid "" 0) (Uid "" 1))
                 (example-foo 'x 'y)))
