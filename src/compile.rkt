#lang typed/racket/no-check

(require
 "./configuration.rkt"
 "./logging.rkt"
 "./grift/reduce-to-cast-calculus.rkt"
 "./casts/impose-cast-semantics.rkt"
 "./backend/code-generator.rkt"
 racket/logging)

(provide (all-defined-out)
         (all-from-out "./configuration.rkt")
         grift-logger-filter)

;; This is the main compiler it is composed of several micro
;; compilers for successivly lower level languages.
(: compile/current-parameterization : (Path -> Path))
(define (compile/current-parameterization path)
  (let* (;; read(lex), parse, typecheck, insert-implicit-casts
         [c0  : Cast0-Lang  (reduce-to-cast-calculus path)]
         ;; specify behavior/representation of casts, and all language constructs
         [uil  : Data0-Lang (impose-cast-semantics c0)])
    ;; generate machine code
    (generate-code uil)))

;; compile file at path
;; exec-path = path/name of final executable
(: compile (->* ((U String Path))
                (#:output     (Option (U String Path))
                 #:keep-c     (Option (U String Path)) 
                 #:keep-s     (Option (U String Path))
                 #:blame      Blame-Semantics
                 #:cast       Cast-Representation
                 #:cc-opts    (U String (Listof String))
                 #:mem        Natural
                 #:t*-ht-size Natural
                 #:t*-ht-load Flonum
                 #:rt         (Option (U String Path))
                 #:log-level  Log-Level
                 #:log-port   (Option (U String Path Output-Port))
                 #:gc         GC
                 #:ck-bounds  Boolean
                 #:track-vars Boolean
                 #:ref        Ref-Semantics)
                Path))
(define (compile target
                 #:output     [output     (output-path)]
                 #:keep-ir    [keep-ir     (ir-code-path)]
                 #:keep-s     [keep-s     (s-path)]
                 #:blame      [blame      (blame-semantics)]
                 #:cast       [cast       (cast-representation)]
                 #:cc-opts    [cc-opts    (c-flags)]
                 #:mem        [mem        (init-heap-kilobytes)]
                 #:t*-ht-size [t*-ht-size (init-types-hash-table-slots)]
                 #:t*-ht-load [t*-ht-load (types-hash-table-load-factor)]
                 #:rt         [rt         (runtime-path)]
                 #:log-level  [log-level  (grift-log-level)]
                 #:log-port   [log-port   (grift-log-port)]
                 #:gc         [gc         (garbage-collector)]
                 #:ck-bounds  [ckbs       (bounds-checks?)]
                 #:track-vars [track-vars (emit-vars-with-original-source-location?)]
                 #:ref        [rs         (reference-semantics)])
  ;; convert all convience types to internal API
  (let* ([target  (if (string? target) (build-path target) target)]
         [output  (if (string? output) (build-path output) output)]
         [keep-ir (if (string? keep-ir) (build-path keep-ir) keep-ir)]
         [keep-s  (if (string? keep-s) (build-path keep-s) keep-s)]
         [cc-opts (if (string? cc-opts) (list cc-opts) cc-opts)] 
         [rt      (if (string? rt)     (build-path rt)     rt)]
         [log-port : (U #f Output-Port Path)
                   (if (string? log-port) (build-path log-port) log-port)])
    ;; Setup compile time parameters based off of these flags
    (parameterize ([output-path output]
                   [ir-code-path keep-ir]
                   [s-path      keep-s]
                   [blame-semantics blame]
                   [cast-representation cast]
                   [c-flags     cc-opts]
                   [init-heap-kilobytes mem]
                   [init-types-hash-table-slots t*-ht-size]
                   [types-hash-table-load-factor t*-ht-load]
                   [runtime-path rt]
                   [garbage-collector gc]
                   [bounds-checks? ckbs]
                   [reference-semantics rs])
      (define (compile-target) : Path
        (compile/current-parameterization target))
      (define (compile/log-port [p : Output-Port]) : Path
        (parameterize ([grift-log-port p])
          (with-handlers ([exn? (λ (e)
                                  (flush-output p)
                                  (sleep 1)
                                  (flush-output p)
                                  (raise e))])
            (with-logging-to-port p compile-target log-level #:logger grift-logger))))
      (cond
        [(not log-port) (compile-target)]
        [(output-port? log-port) (compile/log-port log-port)]
        [else
         (call-with-output-file log-port #:exists 'replace compile/log-port)]))))

;; We are currently only supporting .grift files
(: grift-path? :  Path -> Boolean)
(define (grift-path? path-searched)
  (equal? #"grift" (filename-extension path-searched)))

(define output-suffix : (Parameterof String) (make-parameter ""))

;; Compile all .grift files in a directory and sub-directories
(: compile-directory : Path -> (Listof Path))
(define (compile-directory compile-dir)
  ;; The directory should exist if we are going to compile it
  (unless (directory-exists? compile-dir)
    (error 'compile-directory "no such directory: ~a" compile-dir))

  (define (path?-valid-dir! [dir : (Option Path)]) : (Option Path)
    (cond
      [(not dir) (void)]
      [(file-exists? dir)
       (error 'compile-directory "not a directory ~a" dir)]
      [(not (directory-exists? dir)) (make-directory* dir)])
    dir)
  
  (define c-dir (path?-valid-dir! (ir-code-path)))
  (define s-dir (path?-valid-dir! (s-path)))
  (define out-suffix (output-suffix))

  ;; filename to output file paths
  (: renaming-output-files : Path -> (Values Path (Option Path) (Option Path))) 
  (define (renaming-output-files file-name)
    (define out-name (path-replace-suffix file-name out-suffix))
    (define c-path (and c-dir (build-path c-dir (path-add-suffix out-name #".c"))))
    (define s-path (and s-dir (build-path s-dir (path-add-suffix out-name #".s"))))
    (values (build-path compile-dir out-name) c-path s-path))
  
  ;; Iterate over all grift files in directory and subdirectories
  (for/list ((fl (find-files grift-path? compile-dir)))

    ;; generate actual file paths instead of directory paths
    (define-values (out-path c-path? s-path?)
      (let ([p? (file-name-from-path fl)])
        (unless (path? p?)
          (error 'grift-path? "this should never happen"))
        (renaming-output-files p?)))

    ;; Use current parameterization plus new file names to compile the file
    (compile fl #:output out-path #:keep-ir c-path? #:keep-s s-path?)))
