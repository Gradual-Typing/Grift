#lang typed/racket/base

(require "unique-counter.rkt"
         "helpers.rkt")

(provide (all-defined-out)
         (all-from-out "unique-counter.rkt"))

(struct Uid ([prefix : String] [suffix : Natural]) #:transparent)

(define-type Uid* (Listof Uid))

(define FIRST-UID-SUFFIX 0)

(: uid->symbol (-> Uid Symbol))
(define (uid->symbol u) (string->symbol (uid->string u)))

(: uid->string (-> Uid String))
(define (uid->string u)
  ;; Rubout all non c identifier characters
  (: help (Char -> Char))
  (define (help c)
    (let ([n (char->integer c)])
      ;;        A-Z          a-z          0-9
      (if (or (<= 65 n 90) (<= 97 n 122) (<= 48 n 57))
          c
          #\_)))
  (string-append
   (list->string (map help (string->list (Uid-prefix u))))
   "_u"
   (number->string (Uid-suffix u))))

;; Are two uid equal?
(: uid=? (-> Uid Uid Boolean))
(define (uid=? [u : Uid] [v : Uid])
  (or (= (Uid-suffix u) (Uid-suffix v))))

(: make-next-uid! : Unique-Counter -> (String -> Uid))
(define ((make-next-uid! uc) s)
  (Uid s (unique-counter-next! uc)))

(: next-uid! (->* ((U String Symbol)) (Unique-Counter) Uid))
(define (next-uid! s [uc : (Option Unique-Counter) (current-unique-counter)])
  (cond
    [(not uc)
     (error 'next-uid "current-unique-counter isn't parameterized in the dynamic extent")]
    [(string? s) (Uid s (unique-counter-next! uc))]
    [else (Uid (symbol->string s) (unique-counter-next! uc))]))

;; If you are in the state monad you can purely allocate and increment
(define-type Nat Natural)
(: uid-state (String -> (State Nat Uid)))
(define (uid-state (name : String))
  (lambda ((s : Natural))
    (values (Uid name s) (add1 s))))

;; If you are in state passing style you
;; could use this to allocate and increment
(: next-uid (-> String Natural (values Uid Natural)))
(define (next-uid prefix suffix) next-uid
  (values (Uid prefix suffix) (add1 suffix)))

;; A simple macro for helpign with state passing style
;; I am trying to move away from this so that code can
;; be less verbose
(define-syntax let-uid*
  (syntax-rules ()
    [(_ (wrong ...) others ...)
     (raise-syntax-error 'let-uid "next must always be an identifier")]
    [(_ next () body ...)(let () body ...)]
    [(_ next ([name0 prefix0] [name* prefix*] ...) body ...)
     (let-values ([(name0 next) (next-uid prefix0 next)])
       (let-uid* next ([name* prefix*] ...)
		 body ...))]))

(: format-uid! : String Uid -> Uid)
(define (format-uid! fmt x)
  (next-uid! (format fmt (Uid-prefix x))))

(: uid<? : Uid Uid -> Boolean)
(define (uid<? x y)
  (and (not (eq? x y)) (< (Uid-suffix x) (Uid-suffix y))))
