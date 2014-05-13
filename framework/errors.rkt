#lang racket

(provide (all-defined-out))

(define (match-pass-error who which val)
  (pass-error who which "unexpected value in conditional" val))

(define (pass-error who which msg . i)
  (let* ((i-str (format "~a" i))
         (i-str (substring i-str 1 (string-length i-str))))
    (error who "[~a] ~a with irritant(s) ~a" which msg i-str)))

(struct exn:Schml exn ())
(struct exn:Schml:Pass exn:Schml ())
(struct exn:Schml:Syntax exn:Schml ())
(struct exn:Schml:Type exn:Schml ())


