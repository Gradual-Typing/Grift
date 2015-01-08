#lang typed/racket/base

(provide main)
;; Doing this allows you to just (require schml)
;; in order to get the the compiler I may move compiler/src/schml
;; to here but I don't know what
#;(require schml/src/compile)
(: main (->* () #:rest Any))
(define (main . args)
  (printf "Hello\n"))
#;(provide (all-from-out schml/src/compile))

