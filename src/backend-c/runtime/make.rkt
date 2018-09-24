#lang racket/base
(require racket/system)
(provide post-installer)

(define (post-installer collections-top-path this-collection-path)
  (define pwd (build-path this-collection-path "src/backend-c/runtime"))
  (current-directory pwd)
  (unless (system "make" #:set-pwd? #t)
    (error (format "Running make failed in ~a" pwd))))
