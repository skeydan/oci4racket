#lang racket
(require racket/date)
(require ffi/unsafe)
(require "constants.rkt")
(require "defs.rkt")
(provide (all-defined-out))

; initialization and cleanup
(define (init #:err_handler (err_handler default_err_handler) #:lib (lib #f) #:env (env 'oci_env_default))
  (initialize err_handler lib env)) 

(define (clean)
  (unless (cleanup) (error "could not clean up OCILIB objects")))

; error handling
(define (default_err_handler err)
  (printf "An error occurred:~nType: ~s~nCode: ORA-~s~nMsg: ~s~nSQL (error at pos ~s): ~s~n"
          (errgettype err)
          (errgetocicode err)
          (errgetstring err)
          (geterrpos (errgetstmt err))
          (getsql (errgetstmt err))))

; events
(define set-event
  (case-lambda ((stmt evt level tracefile)
                (begin (executestmt stmt (string-append "alter session set tracefile_identifier='" tracefile "'"))
                       (set-event stmt evt level)))
               ((stmt evt level) (executestmt stmt (string-append "alter session set events '" (number->string evt) " trace name context forever, level " (number->string level) "'")))))

(define (unset-event stmt evt)
  (executestmt stmt (string-append "alter session set events '" (number->string evt) " trace name context off'")))

