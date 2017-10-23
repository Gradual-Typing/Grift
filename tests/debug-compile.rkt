#lang typed/racket/base
(require racket/cmdline
         "./rackunit.rkt"
         ;;"../src/compile.rkt"
         "../src/helpers.rkt"
         "./test-compile.rkt")

(require/typed "./paths.rkt"
               [test-tmp-path Path]
               [test-suite-path Path])

(define log (open-output-file (build-path test-tmp-path "d.log.txt")
                              #:exists 'replace))

(: cc (-> String Any))
(define (cc path)
  (let ([path (build-path path)])
    (parameterize ([current-log-port log]
                   [traces '(All)])
      (run-tests (test-suite "debug" (test-compile "debug" path (debug)))))))

(unless (directory-exists? test-tmp-path)
    (make-directory test-tmp-path))

(command-line
 #:program "grift-test-runner"
 #:once-each
 [("-r" "--cast-representation") crep
  "specify which cast representation to use (Twosomes or Coercions)"
  (let ((crep (to-symbol crep)))
    (if (or (eq? 'Twosomes crep)
            (eq? 'Coercions crep))
        (compiler-config-cast-representation crep)
        (error 'tests "--cast-representation given invalid argument ~a" crep)))]
  [("-i" "--itermediate-checks")
   "turn on itermediate checks during testing"
   (intermediate-checks? #t)]
 #:args (path)
 (if (string? path)
     (cc path)
     (error 'debug-compile "this should never happen")))


