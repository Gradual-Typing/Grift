#lang typed/racket/base/no-check

(require racket/port
         racket/match
         (rename-in (only-in racket/base char?)
                    [char? r:char?])
         (prefix-in s: "../src/logging.rkt"))

(provide (all-defined-out))

(define-type Test-Value
  (U blame bool int unit float char
     dyn gbox mbox mvect tuple gvect function debug))

(struct not-lbl ([value : String])
  #:transparent)
(struct blame ([static? : Boolean]
	       [lbl : (U not-lbl String False)])
  #:transparent)
(struct bool ([value : Boolean])
  #:transparent)
(struct int ([value : Integer])
  #:transparent)
(struct char ([value : Char]) 
  #:transparent)
(struct float ([value : Float])
  #:transparent)
(struct dyn ()
  #:transparent)
(struct function ()
  #:transparent)
(struct gbox ()
  #:transparent)
(struct gvect ()
  #:transparent)
(struct mbox ()
  #:transparent)
(struct mvect ()
  #:transparent)
(struct unit ()
  #:transparent)
(struct tuple ()
  #:transparent)
(struct debug ()
  #:transparent)

(: value=? (Any Any . -> . Boolean))
(define (value=? x y)
  (: atomic-value=? : Any Any -> Boolean)
  (define (atomic-value=? x y)
    (or (and (blame? x) (blame? y) (blame=? x y))
        (and (unit? x) (unit? y))
        (and (bool? x) (bool? y) (bool=? x y))
        (and (int? x) (int? y) (int=? x y))
        (and (char? x) (char? y) (char=? x y))
        (and (float? x) (float? y) (float=? x y))))
  (or (cond
        [(debug? x) (s:debug y #t)]
        [(debug? y) (s:debug x #t)]
        [else #f])
      (atomic-value=? x y)
      (and (gbox? x) (gbox? y))
      (and (gvect? x) (gvect? y))
      (and (mbox? x) (mbox? y))
      (and (mvect? x) (mvect? y))
      (and (tuple? x) (tuple? y))
      (and (dyn? x) (dyn? y))
      (and (function? x) (function? y))))

(: blame=? (blame blame . -> . Boolean))
(define (blame=? x y)
  (and
   (eq? (blame-static? x) (blame-static? y))
   (let ([x (blame-lbl x)]
	 [y (blame-lbl y)])
     (cond
      [(not-lbl? x) (not (equal? (not-lbl-value x) y))]
      [(not-lbl? y) (not (equal? (not-lbl-value y) x))]
      [(or (not x) (not y)) #t]
      [else
       (and (string? x) (string? y)
            (cond
             [(equal? x y)]
             [(regexp-match y x) #t]
             [else #f]))]))))


(: bool=? (bool bool . -> . Boolean))
(define (bool=? x y)
  (eq? (bool-value x) (bool-value y)))

(: int=? (int int . -> . Boolean))
(define (int=? x y)
  (equal? (int-value x) (int-value y)))

(: char=? (char char -> Boolean))
(define (char=? x y)
  (equal? (char-value x) (char-value y)))

(: float=? (float float -> Boolean))
(define (float=? x y)
  (equal? (float-value x) (float-value y)))

#| capture the output of exp on current-output-port and match
   as if it were returning a value from one of our compiled
   programs.
|#


(: parse-observable (String -> Test-Value))
(define (parse-observable s)
  (define who 'parse-observable)
  (s:debug
   who
   s
   (cond
     [(regexp-match #px".*Int : (-?[0-9]+)" s) =>
      (lambda (r) 
        (int (cast (string->number (cadr (cast r (Listof String)))) Integer)))]
     [(regexp-match #px".*Bool : #(t|f)" s) =>
      (lambda (r)
        (bool (not (equal? "f" (cadr (cast r (Listof String)))))))]
     [(regexp-match #px"Char : (#\\\\\\S+)" s) =>
      (lambda (r)
        (: realize : String -> Any)
        (define (realize c) (call-with-input-string c read))
        (match r
          [(list r (? string? (app realize (? r:char? c)))) (char c)]
          [_ (blame #f (format "parse-observable: failed to parse ~a" s))]))]
     [(regexp-match #px".*Float : (-?\\d+.\\d+)" s) =>
      (lambda (r)
        (define r->f real->double-flonum)
        (define s->n string->number)
        (match r
          [(list _ (? string? (app s->n (? real? (app r->f f)))))
           (float f)]
          [_ (blame #f (format "parse-observable: failed to parse ~a" s))]))] 
     [(regexp-match #rx".*Function : \\?" s) (function)]
     [(regexp-match #rx".*Dynamic : \\?" s) (dyn)]
     [(regexp-match #rx".*GReference : \\?" s) (gbox)]
     [(regexp-match #rx".*GVector : \\?" s) (gvect)]
     [(regexp-match #rx".*GArray : \\?" s) (gvect)]
     [(regexp-match #rx".*MReference : \\?" s) (mbox)]
     [(regexp-match #rx".*MVector : \\?" s) (mvect)]
     [(regexp-match #rx".*Tuple : \\?" s) (tuple)]
     [(regexp-match #rx".*Unit : \\(\\)" s) (unit)]
     [else (blame #f s)])))

(define-syntax-rule (observe exp)
  (let ([s (call-with-output-string 
             (lambda ([p : Output-Port])
               (parameterize ([current-output-port p]
                              [current-error-port p])
                 exp
                 (flush-output p))))])
    (define who 'observe)
    (s:debug who s (parse-observable s))))
