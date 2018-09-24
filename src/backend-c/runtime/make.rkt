#lang racket/base
(require racket/system)
(provide post-installer)

(define (post-installer collections-top-path this-collection-path)
  (current-directory (build-path this-collection-path "src/backend-c/runtime"))
  (system "make" #:set-pwd? #t))
