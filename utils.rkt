#lang racket
(require srfi/13)
(require racket/date)
(provide (all-defined-out))

(define (getmaxlength strings)
  (let loop ((lst strings) (max 0))
    (if (null? lst) max
        (if (> (string-length (first lst)) max) (loop (rest lst) (string-length (first lst)))
            (loop (rest lst) max)))))

(define (string-list->fixed-width-array width items)
   (string-join (map (lambda (i) (string-pad-right i width #\null)) items)))

(define (gen-output-string width)
  (string-pad-right "" width #\null))
  
; Logging
(define log-bits
  '((log-sql . 0) (log-binds . 1) (log-fetches . 2)))

(define log-level (make-parameter 0))

(define logfile (make-parameter (build-path (current-directory) "ocirkt.log")))

(define default-date-format (make-parameter (list "yyyy-mm-dd hh24:mi:ss" 30)))

(define default-timestamp-format (make-parameter (list "yyyy-mm-dd hh24:mi:ss" 30 0)))

(define (curdate) (parameterize ((date-display-format 'iso-8601)) (date->string (current-date) #t)))

(define (log-sql sql func)
  (begin (when (bitwise-bit-set? (log-level) (cdr (assoc 'log-sql log-bits)))
           (with-output-to-file (logfile)
             (lambda () (printf "~a: ~a: ~a~n" (curdate) func sql))
             #:exists 'append))
         sql))

(define (log-bind name data func)
  (begin (when (bitwise-bit-set? (log-level) (cdr (assoc 'log-binds log-bits)))
           (with-output-to-file
               (logfile)
             (lambda () (printf "~a: ~a: ~a=~a~n" (curdate) func name data))
             #:exists 'append))
         data))

(define (log-fetch name data func)
  (begin (when (bitwise-bit-set? (log-level) (cdr (assoc 'log-fetches log-bits)))
           (with-output-to-file
               (logfile)
             (lambda () (printf "~a: ~a: ~a=~a~n" (curdate) func name data))
             #:exists 'append))
         data))








