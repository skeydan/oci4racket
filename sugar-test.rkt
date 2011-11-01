#lang racket
(require ffi/unsafe)
(require rackunit)
(require rackunit/text-ui)
(require "defs.rkt")
(require "sugar.rkt")

(define conn #f)
(define stmt #f)

(define admin-test
 (test-suite
  "Events"
  #:before (lambda () (init) (set! conn (connect "orcl" "hr" "hr" 'oci_session_default)) (set! stmt (createstmt conn)))
  #:after (lambda () (stmtfree stmt) (connfree conn) (clean))
  (test-case
   "set trace"
   (check-true (set-event stmt 10053 1 "admin-test"))
   (executestmt stmt "select * from user_tables")
   (check-true (unset-event stmt 10053)))))

(for-each (lambda (test) (run-tests test)) (list admin-test
                                                 ))