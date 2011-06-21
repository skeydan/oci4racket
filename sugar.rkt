#lang racket
(require ffi/unsafe)
(require "constants.rkt")
(require "defs.rkt")
(provide init clean)

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

; 


