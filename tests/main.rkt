#lang typed/racket/base

(require "./rackunit.rkt"
         "../src/helpers.rkt"
         "./test-compile.rkt"
         "./paths.rkt"
         racket/cmdline)


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
  `((all     . ,all-tests)
    (most    . ,most-tests)
    (core    . ,core-tests)
    (box     . ,box-tests)
    (vector  . ,vector-tests)
    (tool    . ,tool-tests)
    (program . ,program-tests)
    (large   . ,large-tests)))

;; Parse the command line arguments
(command-line
 #:program "schml-test-runner"
 #:once-each
 [("-s" "--suite") choice
  "specify suite: all most core box vector tool program large"
  (let* ([s? (assq (to-symbol choice) suite-choices)])
    (if s?
        (suite (cdr s?))
        (error 'tests "--suite given invalid argument ~a" choice)))]
 [("-r" "--cast-representation") crep
  "specify which cast representation to use (Twosomes or Coercions)"
  (let ((crep (to-symbol crep)))
    (if (or (eq? 'Twosomes crep)
            (eq? 'Coercions crep))
        (compiler-config-cast-representation crep)
        (error 'tests "--cast-representation given invalid argument ~a" crep)))]
 #:args () (run-tests (suite)))
