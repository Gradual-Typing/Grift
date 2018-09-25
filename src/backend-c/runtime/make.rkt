#lang racket/base
(require racket/system)
(provide post-installer)

(define (post-installer _collections-top-path this-collection-path)
  (define pwd (build-path this-collection-path "src/backend-c/runtime"))
  (parameterize ([current-directory pwd])
    (unless (system "make" #:set-pwd? #t)
      (printf (format "\nError: Running make failed in ~a" pwd)))))
