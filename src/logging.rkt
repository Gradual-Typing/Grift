#lang racket/base

(require (for-syntax racket/base
                     syntax/location)
         file/glob
         racket/logging
         racket/path
         racket/port
         racket/string
         syntax/srcloc
         syntax/location
         "configuration.rkt"
         "helpers.rkt")
(provide (all-defined-out))

(define-logger grift)

(define-syntax (current-srcloc-as-string stx)
  (syntax-case stx ()
    [(_) #`(srcloc->string (quote-srcloc #,stx))]))

(define-syntax logging
  (syntax-rules ()
    [(logging n () f a ...) (logging n (All) f a ...)]
    [(logging n (o0 o* ...) f a ...)
     (let ([t? (trace? 'n 'o0 'o* ...)])
       (when t?
         (log-grift-debug "~a: ~a\n\n" 'n f)
         (log-grift-debug (with-output-to-string (lambda () (display a) ...)))))]))

(define (filter-matches? filter filename rpath)
  (or (not filter)
      (and (string? filter)
           (or (and filename (string=? filter filename))
               (and rpath (glob-match? filter rpath))))))

(define-syntax (log-grift-debug-filter stx)
  (syntax-case stx ()
    [(_ e)
     (let* ([filename (syntax-source-file-name stx)]
            [directory (syntax-source-directory stx)]
            [path (and filename directory (build-path directory filename))]
            [filename (and filename (path->string filename))])
       #`(log-grift-debug
          (let* ([thunk (lambda () e)]
                 [filename  #,filename]
                 [path #,path]
                 [srcloc (srcloc->string (quote-srcloc #,stx))]
                 [cpath (current-directory)]
                 [filter (and (or filename path) (grift-logger-filter))]
                 [rpath  (and filter (find-relative-path (simple-form-path cpath)
                                                         (simple-form-path path)))])
            (cond
              [(filter-matches? filter filename rpath)
               (string-join
                (list
                 (format "~a:\n" srcloc)
                 (thunk)))]
              [else (format "filtered: ~a" srcloc)]))))]))

(define (debug-aux ls)
  (define (aux p _)
    (printf "    ~a=~v\n" (car p) (cdr p))
    (cdr p))
  (foldl aux void ls))

(define-syntax (debug stx)
  (syntax-case stx (off std)
    [(_ off e* ... e) #`(begin e* ... e)]
    [(_ std e* ... e)
     #`(begin
         (printf "~a:\n" (srcloc->string (quote-srcloc #,stx)))
         (debug-aux `((e* . ,e*) ... (e . ,e))))]
    [(_ e* ... e)
     (with-syntax ([(t* ... t) (generate-temporaries #'(e* ... e))])
       #`(let ([t* e*] ... [t e])
           #,(syntax/loc stx
               (log-grift-debug-filter
                (with-output-to-string
                  (lambda ()
                    (debug-aux `((e* . ,t*) ... (e . ,t)))))))
           t))]
    [other (error 'debug "invalid syntax")]))

;; Code copying is dumb but I couldn't figure out how to
;; use the macros in both typed and untyped.
;; Should I look at rewriters?
(module typed typed/racket/base
  (require (for-syntax racket/base)
           racket/logging
           racket/port
           syntax/location)
  (require/typed racket/base
    [srcloc->string (srcloc -> (Option String))])
  (provide (all-defined-out))
  (define-logger grift)
  (define-syntax logging
    (syntax-rules ()
      [(logging n () f a ...) (logging n (All) f a ...)]
      [(logging n (o0 o* ...) f a ...)
       (let ([t? (trace? 'n 'o0 'o* ...)])
         (when t?
           (log-grift-debug "~a: ~a\n\n" 'n f)
           (log-grift-debug
            (with-output-to-string (lambda () (display a) ...)))))]))

  (define-syntax (debug stx)
    (syntax-case stx (off std)
      [(debug off e* ... e)
       #'(begin e* ... e)]
      [(debug std e* ... e)
       (with-syntax ([(t* ... t) (generate-temporaries #'(e* ... e))])
         #`(let ([t* e*] ... [t e])
             (printf "~a:\n" (srcloc->string (quote-srcloc #,stx)))
             (printf "  ~a=~v\n" 'e* t*)
             ...
             (printf "  ~a=~v\n" 'e t)
             (newline)
             t))]
      [(_ e* ... e)
       #'(let ([ret #f])
           (log-grift-debug
            (with-output-to-string
              (set! ret (debug std e* ... e))))
           ret)]
      [other (error 'debug "invalid syntax")])))





