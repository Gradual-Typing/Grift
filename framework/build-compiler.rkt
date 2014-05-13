#lang racket
(require Schml/framework/errors)
(provide compiler-config
         compose-compiler
         define-pass
         debug?)


(define-syntax compose-compiler
  (syntax-rules ()
    [(_ (e s))
     (raise-syntax-error 'compose-compiler)]
    [(_ (e s) p0) (p0 e s)]
    [(_ (e s) p0 p1 p* ...)
     (let ((e^ (p0 e s)))
       (compose-compiler (e^ s) p1 p* ...))]))

;; I will likely want to come back and override these settings so that
;; it is easy to display and store specific setting.

(struct compiler-config
        (cast-semantics
         trace-passes
         type-checks))


(define (debug? pass pass*)
  (and (not (eq? 'none pass*))
       (or (eq? 'all pass*)
           (and (pair? pass*)
                (ormap (lambda (p) (eq? pass p)) pass*)))))

#|
Macro: define-pass offers syntax magic over pass definitions
where passes automatic have extra debugging features built in
and performed by according to the settings in the compiler
configuration stuct.
|#
(define-syntax define-pass
  (lambda (stx)
    (syntax-case stx (->)
      [(_ (name tree config) (L1? -> L2?)
          decls ... expr)
       (with-syntax ((pass (datum->syntax #'name 'pass)))
         #'(define (name tree config)
             (let ((pass 'name)
                   (traces (compiler-config-trace-passes config))
                   (tycks (compiler-config-type-checks config)))
               (when (debug? pass traces)
                 (printf "Pass ~a input:\n ~a\n" pass tree))
               (when (and (debug? pass tycks)
                          (not (L1? tree)))
                 (pass-error pass 'pre-pass-type-check tree))
               (let ()
                 decls ...
                 (let ((result expr))
                   (when (debug? pass traces)
                     (printf "Pass ~a output:\n ~a\n" pass result))
                   (when (and (debug? pass tycks)
                              (not (L2? result)))
                     (pass-error pass 'post-pass-check result))
                   result)))))])))
