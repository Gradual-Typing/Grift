#lang typed/racket/base

(provide (all-defined-out))
(require (for-syntax typed/racket/base)
         racket/match)

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
       (when t? (logf (format "~a: ~a\n\n" 'n f) a ...)))]))

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

(define-syntax trace-define
  (syntax-rules (->)
    [(_ (f i ...) -> (r ...) e ... b)
     (begin
       (define n (box 0))
       (define (f i ...)
         (define t? (trace? 'All 'f))
         (set-box! n (+ (unbox n) 1))
         (when t?
           (logf "~a@~a:~a=~v\n" 'f (unbox n) 'i i) ...
           (logf "\n")
           (flush-output (current-log-port)))
         e ...
         (let-values ([(r ...) b])
           (set-box! n (- (unbox n) 1))
           (when t?
             (logf "~a@~a:~a=~v\n" 'f (unbox n) 'r r) ...
             (logf "\n")
             (flush-output (current-log-port)))
           (values r ...))))]
    [(_ (f i ...) e ... b)
     (trace-define (f i ...) -> (out) e ... b)]))

;; allows for conditional compilation
(define-for-syntax under-construction?
  (and (getenv "griftUnderConstruction") #t))

(define-for-syntax syntax-id
  (syntax-rules ()
    [(_ x ...) (begin x ...)]))

(define-for-syntax syntax-void
  (syntax-rules ()
    [(_ x ...) (void)]))

;; only run this code if we are working on grift
(define-syntax if-in-construction
  (if under-construction?
      syntax-id
      syntax-void))

(struct exn:todo exn ())
(begin-for-syntax (struct exn:todo exn ()))

(define-syntax TODO
  (if under-construction?
      (syntax-rules ()
        [(_ x ...)
         (raise
          (exn:todo
           (format "TODO: ~a" '(x ...))
           (current-continuation-marks)))])
      (lambda (x)
        (define loc-string
          (srcloc->string
           (srcloc (syntax-source x) (syntax-line x) (syntax-column x)
                   (syntax-position x) (syntax-span x))))
        (raise-syntax-error
         'Unfinished-TODO
         (format "~a: ~a" loc-string (syntax->datum x))))))

;; Do syntax gives an imperative style syntax to monadic code
;; This particular implementation extends this were useful allowing
;; the user to use other binding an branching form while
;; maintaining the "do syntax" in the sytactic conclusions of the forms.
;; The common form:
#;(do (bind-operation : (Monad-name Impure-Type Pure-Type)
    ;; Monadic Statements
    (pattern : Pure-Type <- monadic-expression))
    ;; Or discarding the pure value
    (monadic-expression : Monadic-Type)
    ...
    monadic-expression)
;; TODO syntax case may allow for more parametricity over other
;; binding forms
(define-syntax do
  (syntax-rules (<- : doing
                 let let* let-values let*-values match-let match-let-values
                 match match*
                 begin if
                 )
    ;; let let* let-values let*-values match-let match-let-values
    ;; all work in do expression with the bodies of the let implicitly
    ;; acting as a do expression
    [(_ ann (let (b ...) e e* ...))
     (let (b ...) (do ann e e* ...))]
    [(_ bind (let* (b ...) e e* ...)) (let* (b ...) (do bind e e* ...))]
    [(_ bind (let-values (b ...) e e* ...))
     (let-values (b ...) (do bind e e* ...))]
    [(_ bind (let*-values (b ...) e e* ...))
     (let*-values (b ...) (do bind e e* ...))]
    [(_ bind (match-let (b ...) e e* ...))
     (match-let (b ...) (do bind e e* ...))]
    [(_ bind (match-let-values (b ...) e e* ...))
     (match-let-values (b ...) (do bind e e* ...))]
    ;; match and match* also work in do-expressions
    ;; the right hand side of each match clause is implicitly
    ;; a do expression
    [(_ ann (match  e [p0 s0 s0* ...] [p s s* ...] ...))
     (match  e  [p0 (do ann s0 s0* ...)] [p (do ann s s* ...)] ...)]
    [(_ ann (match* vs [p0 s0 s0* ...] [p s s* ...] ...))
     (match* vs [p0 (do ann s0 s0* ...)] [p (do ann s s* ...)] ...)]
    ;; Facilitate faster type-checking around ifs (maybe) and
    ;; allow for un-annotate do-syntax in if branches with the help
    ;; of the "Infer" rule below.
    [(_ ann (if t c a)) (if t (do ann c) (do ann a))]
    ;; Allow begin to escape to real effects
    ;; Useful for printing because the IO monad would be too much
    [(_ ann (begin s! ...) s s* ...) (begin s! ... (do ann s s* ...))]
    ;; "Infer" the type of unannotated do expressions
    ;; by copying the current bind-operation and type annotation.
    [(_ (bind : T) (doing (p : t <- rhs) s* ...))
     (do (bind : T) (p : t <- rhs) s* ...)]
    ;; Left hand side of each do statement is actually a pattern
    ;; TODO amk find a way to give this better error messages
    ;; like below but more abstract
    [(_ ann ((p ...) : a <- e0) s s* ...)
     (do ann (tmp : a <- e0) (match-let ([(p ...) tmp]) s s* ...))
     #|
     (bind (ann e0 (C m a))
     (lambda ((tmp : a)) : (C m b)
     (match tmp
       [(p ...) (do (bind : (C m b)) e e* ...)]
       [otherwise
        (error 'do "pattern ~a didn't match ~a" '(p ...) tmp)])))
     |#]
    ;; Hint the type of do expressions by annotating the return value
    [(_ (bind : T) e) (ann e : T)]
    ;; Normal definition of do but propogating types to help the
    ;; type checker.
    [(_ (bind : (C m b)) (v : a <- e0) s s* ...)
     (ann
      (bind (ann e0 (C m a))
       (lambda ([v : a])
         : (C m b)
         (do (bind : (C m b)) s s* ...)))
      (C m b))]
    ;; More concise syntax for throwing away the pure value.
    [(_ (bind : (C m b)) (e0 : T) s s* ...)
     (bind (ann e0 T) (lambda (_) (do (bind : (C m b)) s s* ...)))]))


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

(define #:forall (M A B C)
  (map-state2 [f : (A B -> (State M C))] [l1 : (Listof A)] [l2 : (Listof B)])
  : (State M (Listof C))
  (letrec ([loop : ((Listof A) (Listof B) -> (State M (Listof C)))
            (lambda ([l1 : (Listof A)]
                     [l2 : (Listof B)])
              : (State M (Listof C))
              (cond
                [(null? l1)
                 (if (null? l2)
                     (return-state '())
                     (error 'map-state2 "second list longer"))]
                [(null? l2) (error 'map-state "first list longer")]
                [else
                 (do (bind-state : (State M (Listof C)))
                     (a : C <- (f (car l1) (car l2)))
                     (d : (Listof C) <- (loop (cdr l1) (cdr l2)))
                     (return-state (cons a d)))]))])
    (loop l1 l2)))


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



#| Utilities Not Provided by Racket |#

;; Binding While Branching!
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

;; Cast to Boolean
(define (true? [x : Any]) : Boolean
  (if x #t #f))

(: to-symbol (Any -> Symbol))
(define (to-symbol a)
  (cond
    [(string? a) (string->symbol a)]
    [(symbol? a) a]
    [else (to-symbol (format "~a" a))]))
