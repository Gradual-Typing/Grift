#lang typed/racket

(require "./configuration.rkt"
         "./helpers.rkt"
         "./schml/reduce-to-cast-calculus.rkt"
         "./casts/impose-cast-semantics.rkt"
         "./data/convert-representation.rkt"
         "./backend-c/code-generator.rkt")

(provide (all-defined-out))
(provide (struct-out Config))

;; Default places for everything, but there is no default source
(define c-path : (Parameterof Path)
  (make-parameter (build-path "a.c")))
(define keep-c? : (Parameterof Boolean)
  (make-parameter #f))
(define asm-path : (Parameterof (Option Path))
  (make-parameter #f))
(define target-path : (Parameterof Path)
  (make-parameter (build-path "a.out")))
(define log-path : (Parameterof (Option Path))
  (make-parameter #f))
(define mem-dflt : (Parameterof (Option Natural))
  (make-parameter 100000000))

;; Semantic Option
(define semantics : (Parameterof Semantics)
  (make-parameter 'Lazy-D))

;; Cast Represtentation
(define cast-rep : (Parameterof Cast-Representation)
  (make-parameter 'Twosomes))

;; Interaction with the c compiler
(define c-flags : (Parameterof (Listof String))
  (make-parameter '()))

;; This is the main compiler it is composed of several micro
;; compilers for successivly lower level languages.
(: compile/conf (Path Config . -> . Path))
(trace-define (compile/conf path config)
  (let* (;; read(lex), parse, typecheck, insert-implicit-casts
         [c0  : Cast0-Lang  (reduce-to-cast-calculus path config)]
         ;; specify behavior/representation of casts, and all language constructs
         [d0  : Data0-Lang (impose-cast-semantics c0 config)]
         ;; change how the language represents expressions to get a c like ast
         [uil : Data5-Lang (convert-representation d0 config)])
    ;; generate c code and compile it
    (c-backend-generate-code uil config)))

;; compile file at path
;; exec-path = path/name of final executable
(: compile (->* ((U String Path))
                (#:semantics Semantics
                 #:output Path
                 #:keep-c Path
                 #:keep-a Path
                 #:cc-opt (U String (Listof String))
                 #:log    Path
                 #:mem    Natural
                 #:cast-rep Cast-Representation
                 #:rt Path)
                Path))
(define (compile path
                 #:semantics [smtc (semantics)]
                 #:output    [outp (target-path)]
                 #:keep-c    [kp-c : (Option Path) (if (keep-c?) (c-path) #f)]
                 #:keep-a    [kp-a : (Option Path) (asm-path)]
                 #:cc-opt    [opts : (U String (Listof String)) (c-flags)]
                 #:log       [logp : (Option Path) (log-path)]
                 #:mem       [mem :  (Option Natural) (mem-dflt)]
                 #:cast-rep  [crep (cast-rep)]
                 #:rt        [rt  : (Option Path) #f])
  (let* ([path  (simple-form-path (if (string? path)
                                      (string->path path)
                                      path))]
         [opts (if (string? opts) (list opts) opts)]
         [cpth (or kp-c (c-path))]
         [kp-c (true? kp-c)]
         [mem (if (number? mem) mem 0)])
    
    (when logp
      (current-log-port (open-output-file logp #:exists 'replace #:mode 'text)))
    (parameterize ([print-as-expression #t] [print-graph #t] [print-struct #t])
      (compile/conf path (Config path smtc outp cpth kp-c opts kp-a crep mem rt)))))


(: envoke-compiled-program
   (->* () (#:exec-path (Option Path) #:config (Option Config)) Boolean))
(define (envoke-compiled-program #:exec-path [path #f] #:config [config #f])
  (cond
    [config (system (path->string (Config-exec-path config)))]
    [path   (system (path->string path))]
    [else   (system (path->string (target-path)))]))

