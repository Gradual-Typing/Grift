#lang racket
(require Schml/language/syntax
         Schml/framework/build-compiler)
(provide read)

(define-pass (read file-path settings)
  (path? -> lang-syntax?)
  (parameterize
        ([read-case-sensitive #t]
         [read-square-bracket-as-paren #t]
         [read-curly-brace-as-paren #t]
         [read-accept-box #f]
         [read-accept-compiled #f]
         [read-accept-bar-quote #f]
         [read-accept-graph #f]
         [read-decimal-as-inexact #f]
         [read-accept-dot #f]
         [read-accept-infix-dot #f]
         [read-accept-quasiquote #f]
         [read-accept-reader #f]
         [read-accept-lang #f])
    (call-with-input-file file-path
      (lambda (p)
        (let* ((file-name (path->string (file-name-from-path file-path)))
               (read (lambda (p) (read-syntax file-name p))))
          (port-count-lines! p)
          (File file-name (for/list ((stx (in-port read p))) stx))))
      #:mode 'text)))



