#lang racket
(require ffi/unsafe)
(provide (all-defined-out))

(define _ocilib-env-type
  (_bitmask '(oci_env_default = 0
              oci_env_threaded = 1
              oci_env_context = 2
              oci_env_events = 4)))

;(define _ocilib-env-type
;  (_enum '(oci_env_default = 0
;              oci_env_threaded = 1
;              oci_env_context = 2
;              oci_env_events = 4)))

(define _ocilib-charset-type
  (_enum '(oci_char_ansi = 1
              oci_char_wide = 2)))

(define _ocilib-session-type
  (_enum '(oci_session_default = 0
              oci_session_xa = 1
              oci_session_sysdba = 2
              oci_session_sysoper = 4
              oci_session_prelim_auth = 8)))

(define _ocilib-error-type
  (_enum '(oci_err_oracle = 1
              oci_err_ocilib = 2
              oci_err_warning = 3)))

(define _ocilib-timestamp-type
  (_enum '(oci_timestamp = 1
              oci_timestamp_tz = 2
              oci_timestamp_ltz = 3)))

