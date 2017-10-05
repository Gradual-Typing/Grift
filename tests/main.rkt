#!/usr/bin/env racket
#lang racket/base

(require rackunit
         rackunit/text-ui
         racket/sequence
         racket/function
         racket/file
         racket/format
         racket/system
         racket/match
         (except-in "./test-compile.rkt" debug)
         "./paths.rkt"
         "./test-compile-file.rkt"
         "../src/compile.rkt"
         "../src/logging.rkt"
         racket/cmdline
         racket/path)


(provide (all-defined-out))

(require "./suite/tests.rkt")

#| The main of this file is inlined |#

;; make sure that there is an unobtrusive place for temp files
(unless (directory-exists? test-tmp-path)
  (make-directory test-tmp-path))

(define log
  (open-output-file (build-path test-tmp-path "t.log.txt")
                    #:mode 'text #:exists 'replace))

;; Allow choice of test suites
(define suite (make-parameter most-tests))

(define suite-choices 
  `((all     . ,all-tests)
    (most    . ,most-tests)
    (static  . ,statically-typed-gradual-tests)
    (core    . ,core-tests)
    (box     . ,box-tests)
    (monobox . ,monobox-tests)
    (monovector . ,monovector-tests)
    (vector  . ,vector-tests)
    (tuples  . ,tuple-tests)
    (tool    . ,tool-tests)
    (program . ,program-tests)
    (large   . ,large-tests)))

;; The value of the test-cast-representations parameter
;; before alterations to the configuration.
(define default-test-cast-representation
  '(Static |Type-Based Casts| Coercions Hyper-Coercions))

;; Parameter Specifying which cast-representation variables
;; get tested when running the test suite
(define test-cast-representation
  (make-parameter default-test-cast-representation))
(define test-blame-semantics
  (make-parameter '(Lazy-D)))

(define test-dynamic-operations
  (make-parameter '(#f #t inline)))

(define test-specialize-cast-code-generation
  (make-parameter '(#f #t)))

(define test-init-heap-kilobytes
  (make-parameter (list (expt 1024 2))))

(define test-c-flags
  (make-parameter
   '(("-Wno-int-conversion" "-Wno-format" "-Wno-unused-value")
     ("-O3" "-Wno-int-conversion" "-Wno-format" "-Wno-unused-value"))))

(define test-suite-dir (make-parameter test-suite-path))

(define (run-test-suite)
  (define dir  (build-path (test-suite-dir)))
  (debug off dir)
  (run-tests
   (make-test-suite
    "/"
    (for*/list ([p (in-directory dir)]
                [e (in-value (debug off (path-get-extension p)))] 
                #:when (and e (equal? e #".schml")))
      (test-path dir p)))))

(define (run-single-path p)
  (define dir  (build-path (test-suite-dir)))
  (for ([cast-rep (test-cast-representation)])
    (parameterize ([cast-representation cast-rep]
                   [output-path (build-path test-tmp-path "t.out")]
                   [c-path (build-path test-tmp-path "t.c")]
                   [c-flags (cons "-O3" (c-flags))]
                   [specialize-cast-code-generation? #t]
                   [check-asserts? #t])
      (run-tests (test-path dir p)))))

(define (test-path suite-dir p)
  (define-values (base src-name _) (split-path p))
  (define print-path
    (let ([cop (current-output-port)])
      (thunk (fprintf cop "\t~a\n" (find-relative-path suite-dir p)))))
  (define tmp-dir (build-path base "tmp"))
  (unless (directory-exists? tmp-dir)
    (make-directory tmp-dir))
  (define tmp-exe (build-path tmp-dir (path-replace-suffix src-name ""))) 
  (define input (file?->string (path-replace-extension p #".in") ""))
  (define out-rx
    (pregexp (file?->string (path-replace-extension p #".out.rx") "")))
  (define err-rx
    (pregexp (file?->string (path-replace-extension p #".err.rx") "^$")))
  (make-test-suite
   (path->string (find-relative-path suite-dir p))
   (for*/list ([cast (in-list (test-cast-representation))])
     (test-suite
      (~a (list cast))
      #:before print-path
      (check-io
       (thunk*
        (system*
         (compile
          p
          #:output  tmp-exe
          #:keep-c  (path-replace-suffix tmp-exe ".c")
          #:keep-s  (path-replace-suffix tmp-exe ".s")
          #:cast   cast)))
       input out-rx err-rx)))))

(define (file?->string p d)
  (cond
    [(file-exists? p) (file->string p)]
    [else d]))

(define (old-run-tests)
  (for ([cast-rep (test-cast-representation)])
    (parameterize ([cast-representation cast-rep]
                   [output-path (build-path test-tmp-path "t.out")]
                   [c-path (build-path test-tmp-path "t.c")]
                   [c-flags (cons "-O3" (c-flags))]
                   [specialize-cast-code-generation? #t]
                   [check-asserts? #t])
      (printf "~a tests running:\n" cast-rep)
      (match cast-rep
        ['Static (run-tests static-tests)]
        [_ (run-tests (suite))]))))

;; Parse the command line arguments
(define main-function (make-parameter old-run-tests))

(command-line
 #:program "schml-test-runner"
 #:once-any
 [("-t" "--test") path-to-test
  "run a single test"
  (define p (build-path path-to-test))
  (unless (file-exists? p)
    (error 'schml-test-runner "invalid path to test: ~v" path-to-test))
  (main-function (thunk (run-single-path p)))]
 [("-d" "--dir") dir
  "run all files in a directiory"
  (unless (directory-exists? dir)
    (error 'schml-test-runner "invalid directory ~v" dir))
  (main-function run-test-suite)
  (test-suite-dir (build-path dir))]
 [("-s" "--suite") choice
  "specify suite: all most core box vector tool program large"
  (let* ([s? (assq (string->symbol choice) suite-choices)])
    (if s?
        (suite (cdr s?))
        (error 'tests "--suite given invalid argument ~a" choice)))]
 ;; Compiler Configuration
 #:multi
 [("-R" "--cast-representation") crep
  "add a cast representation to the tests"
  (let ((crep (string->symbol crep)))
    (case crep 
      [(Static |Type-Based Casts| Coercions Hyper-Coercions)
       (define current-test-cast-representation (test-cast-representation))
       (test-cast-representation
        (if (eq? default-test-cast-representation
                 current-test-cast-representation)
            ;; Reset to empty and add
            (list crep)
            (cons crep current-test-cast-representation)))]
      [else 
       (error 'tests
              "--cast-representation given invalid argument ~a" crep)]))]
 ;; Compiler Configuration -> GC Selection
 #:once-any
 ["--Boehm" "Use Boehm Conservative Collector" (garbage-collector 'Boehm)]
 ["--No-GC" "Do not Collect Garbage"           (garbage-collector 'None)]
 #:args ()
 (debug off (test-suite-dir) (test-cast-representation))
 ((main-function)))
