#lang typed/racket

(require "forms.rkt"
         "../configuration.rkt"
         syntax/location)
(require/typed racket/base
    [srcloc->string (srcloc -> (Option String))])
(provide (all-defined-out))

(define-syntax (begin$ stx)
  (syntax-case stx (assign$)
    [(_ b) #'b]
    [(_ (assign$ u e) b* ... b)
     #'(let ([t (next-uid! 'u)])
         (Begin
           (list (Assign t e))
           (let ([u (Var t)])
             (begin$ b* ... b))))]
    [(_ b* ... b) #'(Begin `(,b* ...) b)]))

(define-syntax (let$ stx)
  (syntax-case stx ()
    [(_ ([i* e*] ...) b* ... b)
     (with-syntax ([(u* ...) (generate-temporaries #'(i* ...))])
       #'(let ([u* (next-uid! 'i*)] ...)
           (Let `([,u* . ,e*] ...)
             (let ([i* (Var u*)] ...)
               (begin$ b* ... b)))))]))

(define-syntax let*$
  (syntax-rules ()
    [(_ () b* ... b) (begin$ b* ... b)]
    [(_ ([i0 e0] [i* e*] ...) b* ... b)
     (let$ ([i0 e0]) (let*$ ([i* e*] ...) b* ... b))]))

(define-syntax (code$ stx)
  (syntax-case stx ()
    [(_ (p* ...) b* ... b)
     (with-syntax ([(u* ...) (generate-temporaries #'(p* ...))])
       #'(let ([u* (next-uid! 'p*)] ...)
           (Code `(,u* ...)
             (let ([p* (Var u*)] ...)
               (begin$ b* ... b)))))]))

(define-syntax (repeat$ stx)
  (syntax-case stx (:)
    [(_ (i e1 e2) () b* ... b) #'(repeat$ (i e1 e2) (_ (Quote '())) b* ... b)]
    [(_ (i e1 e2) (a e3) b* ... b)
     #'(let ([iu (next-uid! 'i)] [au (next-uid! 'a)])
         (Repeat iu e1 e2 au e3
           (let ([i (Var iu)] [a (Var au)])
             (begin$ b* ... b))))]))

(define-syntax cond$
  (syntax-rules (else)
    [(_ [else e* ... e]) (begin$ e* ... e)]
    [(_ [c e* ... e] [c* e** ...] ...)
     (If c
         (begin$ e* ... e)
         (cond$
          [c* e** ...]
          ...))]))

(define-syntax-rule (case$ e [(p** ...) e*] ... [else d])
  (Switch e `([(,p** ...) . ,e*] ...) d))

;; 
#;
(define-syntax and$
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e* ...) (Op 'and (list e1 (and$ e* ...)))]))

#;
(define-syntax or$
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e* ...) (Op 'or (list e1 (or$ e* ...)))]))

(define-syntax and$
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e* ...) (If e1 (and$ e* ...) (Quote #f))]))

(define-syntax or$
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e* ...) (If e1 (Quote #t) (or$ e* ...))]))

(define-syntax-rule (not$ x) (Op 'not (list x)))

(define-syntax-rule (op=? e1 e2) (Op '= (list e1 e2)))

(define-syntax (precondition$ stx)
  (syntax-case stx ()
    [(_ c b* ... b)
     #'(let ([e (begin$ b* ... b)])
         (if (check-asserts?)
             (Begin
               (list
                (If c
                    (Quote 0)
                    (Blame (Quote (format "runtime precondition failed: ~a" (srcloc->string (quote-srcloc #,stx)))))))
               e)
             e))]))








