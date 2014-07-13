#lang typed/racket

(provide (struct-out Config))

(define-type Semantics (U 'Lazy-D))

(struct: Config [(semantics : Semantics)])



