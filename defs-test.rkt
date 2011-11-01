#lang racket
(require ffi/unsafe)
(require rackunit)
(require rackunit/text-ui)
(require "defs.rkt")
(require "sugar.rkt")
(require "utils.rkt")

(define conn #f)
(define stmt #f)

(define library-metadata-test
 (test-suite 
 "Library metadata"
 (test-case
  "characterset"
  (check-equal? (getcharsetmetadata) 'oci_char_ansi))))

(define connection-test
 (test-suite 
 "Connection handling"
 #:before (lambda () (init))
 #:after (lambda () (clean))
 (test-case
  "connect"
  (set! conn (connect "orcl" "hr" "hr" 'oci_session_default))
  (check-not-false conn)
  (check-true (ping conn))
  (check-equal? (apply string-append (add-between (map number->string (list (getmajorversion conn) (getminorversion conn))) ".")) "11.2")
  (check-not-false (connfree conn)))))

(define execution-test
 (test-suite
  "Statements execution"
  #:before (lambda () (init) (set! conn (connect "orcl" "hr" "hr" 'oci_session_default)) (set! stmt (createstmt conn)))
  #:after (lambda () (stmtfree stmt) (connfree conn) (clean))
  (test-case
   "prepare and execute in 2 steps"
   (check-true (prepare stmt "select * from user_tables"))
   (check-true (execute stmt)))
  (test-case
   "prepare and execute in 1 step"
   (check-true (executestmt stmt "select * from user_tables")))))

(define fetch-test
 (test-suite
  "Result fetching"
  #:before (lambda () (init) (set! conn (connect "orcl" "hr" "hr" 'oci_session_default)) (set! stmt (createstmt conn))
             (executestmt stmt "create table fetchtest (id number, name varchar2(30))")
             (executestmt stmt "insert into fetchtest values (1, 'first')")
             (executestmt stmt "insert into fetchtest values (2, 'second')")
             (executestmt stmt "insert into fetchtest values (3, 'third')")
             (commit conn))
  #:after (lambda () (executestmt stmt "drop table fetchtest") (stmtfree stmt) (connfree conn) (clean))
  (test-case
   "simple fetch with default fetch size"
   (begin (executestmt stmt "select id, name from fetchtest order by id")
          (let ((result (getresultset stmt)))
            (check-true (fetchnext result))
            (check-equal? (getint result 1) 1)
            (check-equal? (getstring result 2) "first")
            (check-true (fetchnext result))
            (check-equal? (getstring2 result "name") "second")
            (check-equal? (getint2 result "id") 2)
            (check-equal? (getrowcount result) (if (> (getfetchsize stmt) 2) 3 2))
            (check-equal? (getcurrentrow result) 2))))
  (test-case
   "change fetch size"
   (begin (setfetchsize stmt 1) (executestmt stmt "select rownum from fetchtest order by 1")
          (let ((result (getresultset stmt)))
            (fetchnext result) (fetchnext result) (check-equal? (getrowcount result) 2))))
  (test-case
   "prefetch size"
   (printf "prefetch size: ~s~n" (getprefetchsize stmt))) ; how (and whether to) test this??
 ))
       
(define bind-test
 (test-suite
  "Bind variables"
  #:before (lambda () (init) (set! conn (connect "orcl" "hr" "hr" 'oci_session_default)) (set! stmt (createstmt conn)) (allowrebinding stmt #t)
             (executestmt stmt "create table bindtest (id number, name varchar2(30))"))
  #:after (lambda () (executestmt stmt "drop table bindtest")
            (stmtfree stmt) (connfree conn) (clean))
  (test-case
   "simple bind vars"
   (prepare stmt "insert into bindtest values (:theid, :thename)")
   (check-true (bindint stmt ":theid" 1) (bindstring stmt ":thename" "first"))
   (check-true (execute stmt))
   (check-true (bindint stmt ":theid" 2) (bindstring stmt ":thename" "second"))
   (check-true (execute stmt))
   (executestmt stmt "select count(*) cnt from bindtest")
   (let ((result (getresultset stmt)))
     (begin (fetchnext result) (check-equal? (getint2 result "cnt") 2))
     (check-true (rollback conn))))
  (test-case
   "array interface"
   (prepare stmt "insert into bindtest values (:theid, :thename)")
   (check-true (setbindsize stmt 10))
   (let* ((thestring "abcdefghijklmnopqrstxyz")
         (intlist (let loop ((lst '()) (cnt 0)) (if (> cnt 9) (reverse lst) (loop (cons cnt lst) (+ 1 cnt)))))
         (strlist (let loop ((lst '()) (cnt 0)) (if (> cnt 9) (reverse lst) (loop (cons (substring thestring 0 (* 2 cnt)) lst) (+ 1 cnt))))))
     (check-true (bindintarray stmt ":theid" intlist 0)) ; pass 0 for normal array (non-PLSQL table)
     (check-true (bindstringarray stmt "thename" strlist))) ; pass 0 for normal array (non-PLSQL table)
     (check-true (execute stmt))
     (commit conn)
     (executestmt stmt "select id, name from bindtest where length(name) = (select max(length(name)) from bindtest)")
     (let ((result (getresultset stmt)))
       (begin (fetchnext result) (check-equal? (getint result 1) 9) (check-equal? (getstring result 2) "abcdefghijklmnopqr"))))
  (test-case 
  "updates via rowid"
  (let* ((stmt2 (createstmt conn))
         (result (and (executestmt stmt2 "select count(*) cnt from bindtest") (getresultset stmt2)))
         (cnt (begin (fetchnext result) (getint2 result "cnt"))))
    (prepare stmt "update bindtest set id = :theid where rowid = :therowid")
    (executestmt stmt2 "select rowid, id from bindtest for update")
    (check-true
      (let loop ((cnt cnt) (result (getresultset stmt2)))
       (if (= 0 cnt)
           (commit conn)
           (begin
             (fetchnext result) (bindstring stmt ":therowid" (getstring2 result "rowid")) (bindint stmt ":theid" (* 10 (getint2 result "id"))) (execute stmt)
             (loop (- cnt 1) result)))))
  (let ((result (and (executestmt stmt2 "select sum(id) from bindtest") (getresultset stmt2))))
    (check-equal? (and (fetchnext result) (getint result 1)) 450))))))
             

(define date&timestamp-test
 (test-suite
  "Dates and timestamps"
  #:before (lambda () (init) (set! conn (connect "orcl" "hr" "hr" 'oci_session_default)))
  #:after (lambda () (connfree conn) (clean))
  (test-case
   "Dates"
   (let ((dtptr (datecreate conn)))
     (let ((res (datefromtext dtptr "2011-05-25" "yyyy-mm-dd")))
       (check-true res)
       (let-values (((year month day) (dategetdate dtptr)))
         (check-equal? 2011 year) (check-equal? 5 month) (check-equal? 25 day)
         (let ((res (datesetdatetime dtptr 2011 9 18 8 22 4)))
           (check-true res)
           (let-values (((year month day hour min sec) (dategetdatetime dtptr)))
             (check-equal? 2011 year) (check-equal? 9 month) (check-equal? 18 day) (check-equal? 8 hour) (check-equal? 22 min) (check-equal? 4 sec)
             (let ((text (datetotext dtptr "yyyy-mm-dd hh24:mi:ss" 30)))
               (check-equal? text "2011-09-18 08:22:04"))
             (let ((res (sysdate dtptr)))
               (let-values (((year month day) (dategetdate dtptr)))
                 (check-equal? (date-day (seconds->date (current-seconds))) day)
                 (check-true (datefree dtptr))))))))))
  (test-case
   "Timestamps"
   (let ((tsptr (timestampcreate conn 'oci_timestamp_tz)))
     (check-equal? (timestampgettype tsptr) 'oci_timestamp_tz)
     (let ((res (systimestamp tsptr)))
       (let-values (((text) (timestamptotext tsptr "DD/MM/YYYY HH24:MI:SS:FF3" 100 3)))
         (display text)
         (check-true (timestampfree tsptr))))))))
  
  
;(run-tests
; (test-suite
;  "Data conversion"
;   #:before (lambda () (init) (set! conn (connect "orcl" "hr" "hr" 'oci_session_default)) (set! stmt (createstmt conn)))
;   #:after (lambda () (connfree conn) (clean))
;   (test-case
;   "number->string"

(parameterize ((log-level (bitwise-ior 1 2 4)))
  (for-each (lambda (test) (run-tests test)) (list library-metadata-test
                                                   connection-test
                                                   execution-test
                                                   fetch-test
                                                   bind-test
                                                   date&timestamp-test
                                                   )))