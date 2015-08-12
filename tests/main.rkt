#lang typed/racket

(require "./rackunit.rkt"
         "../src/helpers.rkt"
         "./test-compile.rkt"
         "./paths.rkt")

(provide (all-defined-out))

(require "./suite/tests.rkt")

#| The main of this file is inlined |#

;; make sure that there is an unobtrusive place for temp files
(unless (directory-exists? test-tmp-path)
  (make-directory test-tmp-path))

(define log
  (open-output-file (build-path test-tmp-path "t.log.txt")
                    #:mode 'text #:exists 'replace))

(current-log-port log)

;; Allow choice of test suites
(define suite : (Parameterof Test)
  (make-parameter most-tests))

(define suite-choices : (Listof (Pair Symbol Test))
  `((all . ,all-tests)
    (most . ,most-tests)
    (core . ,core-tests)
    (boxes . ,boxes-tests)
    (vectors . ,vectors-tests)
    (tools . ,tool-tests)
    (program . ,program-tests)
    (large . ,large-tests)))


(define (to-symbol a) : Symbol
  (cond
    [(string? a) (string->symbol a)]
    [(symbol? a) a]
    [else 'invalid]))

;; Parse the command line arguments
(command-line
 #:program "schml-test-runner"
 #:once-each
 [("-s" "--suite") choice
  "specify which suite to run"
  (let* ([s? (assq (to-symbol choice) suite-choices)])
    (if s?
        (suite (cdr s?))
        (error 'tests "--suite given invalid argument ~a" choice)))]
 #:args () (run-tests (suite)))
