#lang typed/racket/base

;; Doing this allows you to just (require schml)
;; in order to get the the compiler I may move compiler/src/schml
;; to here but I don't know what
(require schml/src/compiler)
(provide (all-from-out schml/src/compiler))
