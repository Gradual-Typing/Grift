#lang typed/racket

(provide (all-defined-out))

#| Environments are persistent hash tables |#

#| Source locations always return a string even if it is empty |#
(require/typed racket/base
	       [(srcloc->string src->str)
		(srcloc . -> . (U False String))])
(: srcloc->string (srcloc . -> . String))
(define (srcloc->string x) (or (src->str x) ""))

(: syntax->srcloc ((Syntaxof Any) . -> . srcloc))
(define (syntax->srcloc x)
  (srcloc (syntax-source x)
	  (syntax-line x)
          (syntax-column x)
	  (syntax-position x)
	  (syntax-span x)))

(: file->srcloc (String . -> . srcloc))
(define (file->srcloc n)
  (srcloc n #f #f #f #f))

#|
A language form ends up just being a polymorphic record.
This allows me to make very desciptive grammars via types later on.
|#

(define-syntax (define-forms stx)
  (syntax-case stx ()
    [(_ (name fields ...) f* ...)
     (with-syntax ([(types ...) (generate-temporaries #'(fields ...))])
       #'(begin
	   (struct (types ...) name ([fields : types] ...) #:transparent)
	   (define-forms f* ...)))]
    [(_) #'(void)]))

(define-syntax-rule (define-type+ id ([id* c*] ...) t)
  (begin (define-type id t)
	 (define-type id* (c* id)) ...))


#| In order to simulate the ability to pass the wrong
   number of arguments to a function I need a fold-right
   that takes the shorter of two lists
|#

(: fold-2-left
   (All (a b c)
	(-> (-> a b c c) c (Listof a) (Listof b) c)))
(define (fold-2-left p acc l0 l1)
  (if (or (null? l0) (null? l1))
      acc
      (fold-2-left p
		   (p (car l0) (car l1) acc)
		   (cdr l0)
		   (cdr l1))))


(: snoc (All (A B) (Listof A) B -> (Listof (U A B))))
(define (snoc l e)
  (match l
    [(cons a d) (cons a (snoc d e))]
    ['() (list e)]))


#| Some helpers for debuging |#

(: traces (Parameter (Listof Symbol)))
(define traces (make-parameter '()))

(: trace (-> (U Symbol (Listof Symbol)) Void))
(define (trace t)
  (let* ((t* (traces)))
    (if (symbol? t)
        (traces (cons t t*))
        (traces (append t t*)))))

(define-syntax-rule (trace? s ...)
  (let ([t*? (traces)])
    (or (memq s t*?) ...)))

(: current-log-port (Parameter Output-Port))
(define current-log-port (make-parameter (current-error-port)))

(define-syntax-rule (logf fmt a ...)
  (let ()
    (fprintf (current-log-port) fmt a ...)
    (flush-output (current-log-port))))


(define-syntax-rule (pass/log (o ...) (p in c ...))
  (let ([t? (trace? 'p 'All 'o ...)])
    (when t? (logf "~v input:\n~v\n\n" 'p in))
    (let ([out (p in c ...)])
      (when t? (logf "~v output:\n~v\n\n" 'p in))
      out)))


(define-syntax logging
  (syntax-rules ()
    [(logging n () f a ...) (logging n (All) f a ...)]
    [(logging n (o0 o* ...) f a ...)
     (let ([t? (trace? 'n 'o0 'o* ...)])
       (when t? (logf (format "~v: ~v\n\n" 'n f) a ...)))]))

(define-syntax-rule (log-body n (v ...) e ... b)
  (let ([t? (trace? 'n 'All)])
    (when t?
      (logf "~v input:\n" 'n)
      (logf "\t~v : ~v\n" 'v v) ...
      (logf "\n"))
    e ...
    (let ([out b])
      (when t?
        (logf "~v output:\n\t~v\n\n" 'n out)
        (flush-output (current-log-port)))
      out)))

(define-syntax-rule (tracef (s ...) fmt a ...)
  (when (trace? s ...)
    (logf fmt a ...)))

;; allows for conditional compilation
(define-for-syntax under-construction?
  (and (getenv "schmlUnderConstruction") #t))

(define-for-syntax syntax-id
  (syntax-rules ()
    [(_ x ...) (begin x ...)]))

(define-for-syntax syntax-void
  (syntax-rules ()
    [(_ x ...) (void)]))

(define-syntax-rule (trace-define (f a ...) e ... b)
  (define (f a ...)
    (define t? (trace? 'All 'f))
    (define n (box 0))
    (when t?
      (logf "~v ~v input:\n" 'f (unbox n))
      (logf "\t~v : ~v\n" 'a a) ...
      (logf "\n")
      (flush-output (current-log-port)))
    (let ()
      e ...
      (let ([out b])
        (when t?
          (logf "~v ~v output: ~v" 'f n out)
          (flush-output (current-log-port))
          (set-box! n (+ (unbox n) 1)))
        out))))

;; An error a syntax tranformer that reports the current macro is undefined
(define-for-syntax syntax-undefined-if-used
  (lambda (stx)
    (syntax-case stx ()
      [(a x ...) (raise-syntax-error 'if-in-construction
                                     "debug macro left in code"
                                     #'(a x ...)
                                     #'a)])))

;; only run this code if we are working on schml
(define-syntax if-in-construction
  (if under-construction?
      syntax-id
      syntax-void))

(if-in-construction
 (provide exn:todo)
 (struct exn:todo exn ()))


(define-syntax TODO
  (if under-construction?
      (syntax-rules ()
        [(_ x ...)
         (raise
          (exn:todo
           (format "TODO: ~a" '(x ...))
           (current-continuation-marks)))])
      syntax-undefined-if-used))

(define-syntax do
  (syntax-rules (<- :
                 let let* let-values let*-values
                 match-let match-let-values)
    [(_ bind (let (b ...) e e* ...))
     (let (b ...) (do bind e e* ...))]
    [(_ bind (let* (b ...) e e* ...))
     (let* (b ...) (do bind e e* ...))]
    [(_ bind (let-values (b ...) e e* ...))
     (let-values (b ...) (do bind e e* ...))]
    [(_ bind (let*-values (b ...) e e* ...))
     (let*-values (b ...) (do bind e e* ...))]
    [(_ bind (match-let (b ...) e e* ...))
     (match-let (b ...) (do bind e e* ...))]
    [(_ bind (match-let-values (b ...) e e* ...))
     (match-let-values (b ...) (do bind e e* ...))]
    [(_ (bind : T) e) (ann e : T)]
    [(_ (bind : (C m b)) ((p ...) : a <- e0) e e* ...)
     (bind (ann e0 (C m a))
           (lambda ((tmp : a)) : (C m b)
            (match tmp
              [(p ...) (do (bind : (C m b)) e e* ...)]
              [otherwise
               (error 'do "pattern ~a didn't match ~a" '(p ...) tmp)])))]
    [(_ (bind : (C m b)) (v : a <- e0) e e* ...)
     (bind (ann e0 (C m a)) (lambda ((v : a)) : (C m b) (do (bind : (C m b)) e e* ...)))]
    [(_ (bind : (C m b)) (e0 : T) e e* ...)
     (bind (ann e0 T) (lambda (_) (do (bind : (C m b)) e e* ...)))]))


#| The state monad |#
(define-type (State S A) (S . -> . (values A S)))

(define #:forall (M A) (run-state (ma : (State M A)) (s : M))
  : (values A M)
  (ma s))

(define #:forall (M A) (return-state (p : A))
  : (State M A)
  (lambda ((i : M)) (values p i)))

(define #:forall (M A B) (bind-state (pi : (State M A))
                                     (f : (-> A (State M B))))
  : (State M B)
  (lambda ((i : M))
    (let-values ([((p : A) (i : M)) (pi i)])
      ((f p) i))))

(define #:forall (M) (put-state (p : M))
  : (State M Null)
  (lambda ((s : M))
    (values '() p)))

(define #:forall (M) get-state : (State M M)
  (lambda ((i : M))
    (values i i)))


(define #:forall (M A B) (map-state [f : (A -> (State M B))] [l : (Listof A)])
  : (State M (Listof B))
  (letrec ([loop : ((Listof A) . -> . (State M (Listof B)))
            (lambda ([l : (Listof A)]) : (State M (Listof B))
             (if (null? l)
                 (return-state '())
                 (do (bind-state : (State M (Listof B)))
                     (a : B <- (f (car l)))
                     (d : (Listof B) <- (loop (cdr l)))
                     (return-state (cons a d)))))])
      (loop l)))

(: foldr-state (All (M A B) ((A B -> (State M B)) B (Listof A) -> (State M B))))
(define (foldr-state fn acc ls)
  (if (null? ls)
      (return-state (ann acc B))
      (do (bind-state : (State M B))
          (match-let ([(cons a d) ls])
            (acc : B <- (foldr-state fn acc d))
            (fn a acc)))))

(: lift-state (All (M A B C D)
               (case-> [(A -> B) (State M A) -> (State M B)]
                       [(A B -> C) (State M A) (State M B) -> (State M C)]
                       [(A B C -> D) (State M A) (State M B) (State M C) -> (State M D)])))
(define lift-state
  (case-lambda
    [(f ma)
     (do (bind-state : (State M B))
         (a : A <- ma)
         (return-state (f a)))]
    [(f ma mb)
     (do (bind-state : (State M C))
         (a : A <- ma)
         (b : B <- mb)
         (return-state (f a b)))]
    [(f ma mb mc)
     (do (bind-state : (State M D))
         (a : A <- ma)
         (b : B <- mb)
         (c : C <- mc)
         (return-state (f a b c)))]))

;; extended conditional macros

(define-syntax-rule (lif (t p) c a)
  (let ((t p)) (if t c a)))

(define-syntax ex-cond
  (syntax-rules (else => let let-values match-let)
    [(_ (else e* ... e))
     (begin e* ... e)]
    [(_ (let (b ...) c c* ...))
     (let (b ...) (ex-cond c c* ...))]
    [(_ (let-values (b ...) c c* ...))
     (let (b ...) (ex-cond c c* ...))]
    [(_ (match-let (b ...) c c* ...))
     (let (b ...) (ex-cond c c* ...))]
    [(_ (p => e) c c* ...)
     (lif (t p) (e t) (ex-cond c c* ...))]
    [(_ (p e* ... e) c c* ...)
     (lif (t p) (begin e* ... e) (ex-cond c c* ...))]))
