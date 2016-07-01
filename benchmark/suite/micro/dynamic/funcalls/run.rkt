#lang racket

(require benchmark "../../../../../src/compile.rkt")

(module+ main
  (mem-dflt (expt 1024 3))
  (command-line
   #:args (number-of-trials)
   ;; Initialize directories
   (unless (directory-exists? data-dir)
     (make-directory data-dir))
   (unless (directory-exists? out-dir)
     (make-directory out-dir))
   (unless (directory-exists? tmp-dir)
     (copy-directory/files src-dir tmp-dir))

   (display "benchmarking function calls\n")

   (define results
     (benchmark
      '(call overhead)
      '((gambit twosomes coercions))
      run-benchmark
      #:build build-benchmark
      #:extract-time parse-timing-results
      #:num-trials number-of-trials
      #:results-file (path data-dir "funcall")))

   (display results)))


(define-runtime-path data-dir "data")
(define-runtime-path out-dir "output")
(define-runtime-path tmp-dir "tmp")

(define (file-name test compiler)
  (case compiler
    [(Gambit) (format "s~a.scm" test)]
    [(Twosomes Coercions) (format "~a.schml" test)]))

(define (schml-tmp test compiler extension)
  (case compiler
    [(Twosomes) (format "~a.twosomes.~a" test extension)]
    [(Coercions) (format "~a.coercions.~a" test extension)]))

(define (path dir test compiler)
  (build-path dir (file-name test compiler)))

(define (build-benchmark test compiler)
  (define out-path (path tmp-dir test compiler))
  (define src-path (path src-dir test compiler))
  (case compiler
    [(Gambit)
     (define out-file (path->string out-path))
     (define src-file (path->string src-path))
     (system* gambit "-exe" "-o" out-file "-cc-options" "-O3" src-file)]
    [(Twosomes Coercions)
     (define tmp-c-path (build-path tmp-dir "c" (schml-tmp test compiler "c")))
     (define tmp-a-path (build-path tmp-dir "c" (schml-tmp test compiler "s")))
     (compile src-path
              #:output out-path
              #:keep-c tmp-c-path
              #:keep-a tmp-a-path
              #:cast-rep compiler
              #:mem (starting-memory)
              #:cc-opts "-O3")]))

(define ((run-benchmark iterations) test compiler)
  (define exe-path (path tmp-dir test compiler))
  (with-input-from-string (~a iterations)
    (system* (path->string exe-path))))








