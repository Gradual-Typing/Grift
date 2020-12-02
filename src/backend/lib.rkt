#lang racket/base

(require
 racket/path
 racket/file)
(provide (all-defined-out))

;; Is path syntactically a directory
(define (directory-path? x)
  (not (file-name-from-path x)))

;; Rewrite all paths to have the correct extension
;; If path is a directory write the default filename there
;; If path is a filename use that filename
;; Otherwise write to the default-path
(define (get-write-file default-path suffix path)
  (normalize-path
   (cond
      [(not path) (path-replace-extension default-path suffix)]
      [(directory-path? path)
       (unless (directory-exists? path)
         (make-directory* path))
          (path-replace-extension
           (build-path path (file-name-from-path default-path))
           suffix)]
      [else path])))

