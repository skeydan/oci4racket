#lang racket
(require ffi/unsafe)
(require ffi/unsafe/define)
(require "constants.rkt")
(require "utils.rkt")
(provide (all-defined-out))

(define-ffi-definer define-racket (ffi-lib #f))

(define-racket scheme_make_byte_string
 (_fun _bytes -> _racket))

(define-syntax def-ocilib
  (syntax-rules (:)
    ((_ rackid id : spec ...) (define rackid (get-ffi-obj 'id ocilib (_fun spec ...))))))

(define ocilib (ffi-lib "libocilib"))

; Initialization
(def-ocilib initialize OCI_Initialize : (err_handler : (_cprocedure (list _pointer) _void)) (lib : _pointer) (env : _ocilib-env-type) -> (result : _bool)) 
(def-ocilib cleanup OCI_Cleanup : -> (result : _bool))
(def-ocilib getcharsetmetadata OCI_GetCharsetMetaData :	-> (type : _ocilib-charset-type))
(def-ocilib getcharsetuserdata OCI_GetCharsetUserData :	-> (type : _ocilib-charset-type))

; Connections
(def-ocilib connect  OCI_ConnectionCreate : (db : _string) (user : _string) (pw : _string) (mode : _ocilib-session-type) -> (conn_ptr : _pointer))
(def-ocilib connfree OCI_ConnectionFree : (conn_ptr : _pointer) -> (result : _bool))
(def-ocilib isconn OCI_IsConnected : (conn_ptr : _pointer) -> (result : _bool))
(def-ocilib ping OCI_Ping : (conn_ptr : _pointer) -> (result : _bool))
(def-ocilib getmajorversion OCI_GetServerMajorVersion : (conn_ptr : _pointer) -> (vno : _uint))
(def-ocilib getminorversion OCI_GetServerMinorVersion : (conn_ptr : _pointer) -> (vno : _uint))
(def-ocilib commit OCI_Commit : (conn_ptr : _pointer) -> (result : _bool))
(def-ocilib rollback OCI_Rollback : (conn_ptr : _pointer) -> (result : _bool))

; Statements
(def-ocilib createstmt OCI_StatementCreate : (conn_ptr : _pointer) -> (stmt_ptr : _pointer))
(def-ocilib stmtfree OCI_StatementFree : (stmt_ptr : _pointer) -> (result : _bool))
(def-ocilib prepare OCI_Prepare : (stmt_ptr s) :: (stmt_ptr : _pointer) (sql : _string = (log-sql s 'prepare)) -> (result : _bool))

(def-ocilib execute OCI_Execute : (stmt_ptr : _pointer) -> (result : _bool))
(def-ocilib executestmt OCI_ExecuteStmt : (stmt_ptr s) :: (stmt_ptr : _pointer) (sql : _string = (log-sql s 'executestmt)) -> (result : _bool)) ; does both of prepare and execute sequentially
(def-ocilib getsql OCI_GetSql : (stmt_ptr : _pointer) -> (text : _string))
(def-ocilib geterrpos OCI_GetSqlErrorPos : (stmt_ptr : _pointer) -> (pos : _uint))	

; Error handling
(def-ocilib errgetstring OCI_ErrorGetString : (error_ptr : _pointer) -> (text : _string))
(def-ocilib errgettype OCI_ErrorGetType : (error_ptr : _pointer) -> (type : _ocilib-error-type))
(def-ocilib errgetocicode OCI_ErrorGetOCICode : (error_ptr : _pointer) -> (code : _int))
(def-ocilib errgetstmt OCI_ErrorGetStatement : (error_ptr : _pointer) -> (stmt_ptr : _pointer))
(def-ocilib errgetinternalcode OCI_ErrorGetInternalCode : (error_ptr : _pointer) -> (code : _int))
(def-ocilib errgetconn OCI_ErrorGetConnection : (error_ptr : _pointer) -> (conn_ptr : _pointer))
(def-ocilib errgetrow OCI_ErrorGetRow : (error_ptr : _pointer) -> (code : _uint))
(def-ocilib errgetlast OCI_GetLastError : (error_ptr : _pointer) -> (code : _int))

; Result sets
(def-ocilib getfetchsize OCI_GetFetchSize : (stmt_ptr : _pointer) -> (size : _uint))
(def-ocilib setfetchsize OCI_SetFetchSize : (stmt_ptr : _pointer) (size : _uint) -> (result : _bool))
(def-ocilib getprefetchsize OCI_GetPrefetchSize : (stmt_ptr : _pointer) -> (size : _uint))
(def-ocilib setprefetchsize OCI_SetPrefetchSize : (stmt_ptr : _pointer) (size : _uint) -> (result : _bool))
(def-ocilib getresultset OCI_GetResultset : (stmt_ptr : _pointer) -> (resultset_ptr : _pointer))
(def-ocilib fetchnext OCI_FetchNext : (resultset_ptr : _pointer) -> (result : _bool))
(def-ocilib getint OCI_GetInt : (resultset_ptr : _pointer) (index : _uint) -> (val : _int) -> (log-fetch index val 'getint))
(def-ocilib getint2 OCI_GetInt2 : (resultset_ptr : _pointer) (colname : _string) -> (val : _int) -> (log-fetch colname val 'getint2))
(def-ocilib getdouble OCI_GetDouble : (resultset_ptr : _pointer) (index : _uint) -> (val : _double) -> (log-fetch index val 'getdouble))
(def-ocilib getdouble2 OCI_GetDouble2 : (resultset_ptr : _pointer) (colname : _string) -> (val : _double) -> (log-fetch colname val 'getdouble2))
(def-ocilib getstring OCI_GetString : (resultset_ptr : _pointer) (index : _uint) -> (val : _string) -> (log-fetch index val 'getstring))
(def-ocilib getstring2 OCI_GetString2 : (resultset_ptr : _pointer) (colname : _string) -> (val : _string) -> (log-fetch colname val 'getstring2))
(def-ocilib getdate OCI_GetDate : (resultset_ptr : _pointer) (index : _uint) -> (val : _pointer) -> (and (log-fetch index (datetotext val (car (default-date-format)) (cadr (default-date-format))) 'getdate) val))
(def-ocilib getdate2 OCI_GetDate2 : (resultset_ptr : _pointer) (colname : _string) -> (val : _pointer) -> (and (log-fetch colname (datetotext val (car (default-date-format)) (cadr (default-date-format))) 'getdate2) val))
(def-ocilib gettimestamp OCI_GetTimestamp : (resultset_ptr : _pointer) (index : _uint) -> (val : _pointer) -> (and (log-fetch index (timestamptotext val (car (default-timestamp-format)) (cadr (default-timestamp-format)) (caddr (default-timestamp-format))) 'gettimestamp) val))
(def-ocilib gettimestamp2 OCI_GetTimestamp2 : (resultset_ptr : _pointer) (colname : _string) -> (val : _pointer) -> (and (log-fetch colname (timestamptotext val (car (default-timestamp-format)) (cadr (default-timestamp-format)) (caddr (default-timestamp-format))) 'gettimestamp2) val))
(def-ocilib getrowcount OCI_GetRowCount : (resultset_ptr : _pointer) -> (count : _uint))
(def-ocilib getcurrentrow OCI_GetCurrentRow : (resultset_ptr : _pointer) -> (no : _uint))
;(def-ocilib getstruct OCI_GetStruct : (resultset_ptr structtype indtype) :: (resultset_ptr : _pointer) (struct_ptr : (_ptr o structtype)) (ind_ptr : (_ptr o indtype)) -> (result : _bool) -> (and result (values struct_ptr ind_ptr))) ; does not work

; Bind variables
(def-ocilib allowrebinding OCI_AllowRebinding : (stmt_ptr : _pointer) (val : _bool) -> (result : _bool))
(def-ocilib bindint OCI_BindInt	: (stmt_ptr n d) :: (stmt_ptr : _pointer) (name : _string = n) (data : (_ptr i _int) = (log-bind n d 'bindint)) -> (result : _bool))
(def-ocilib bindstring OCI_BindString : (stmt_ptr n d) :: (stmt_ptr : _pointer) (name : _string = n) (data : _string = (log-bind n d 'bindstring)) (len : _int = (string-length d)) -> (result : _bool))
(def-ocilib setbindsize OCI_BindArraySetSize : (stmt_ptr : _pointer) (size : _uint) -> (result : _bool))
(def-ocilib bindintarray OCI_BindArrayOfInts : (stmt_ptr : _pointer) (name : _string) (data : (_list i _int)) (no : _uint) -> (result : _bool))
(def-ocilib bindstringarray OCI_BindArrayOfStrings : (stmt_ptr name strlist) :: (stmt_ptr : _pointer) (name : _string) (data : _string = (string-list->fixed-width-array (getmaxlength strlist) strlist)) (maxstrlen : _uint = (getmaxlength strlist)) (no : _uint = 0) -> (result : _bool))

; Dates, timestamps and intervals
(def-ocilib datecreate OCI_DateCreate : (conn_ptr : _pointer) -> (date_ptr : _pointer))	
(def-ocilib datefree OCI_DateFree : (date_ptr : _pointer) -> (result : _bool))	
(def-ocilib datefromtext OCI_DateFromText : (date_ptr : _pointer) (strval : _string) (fmt : _string) -> (result : _bool))
; allocate a buffer with malloc, convert to scheme type with cast
(def-ocilib datetotext OCI_DateToText : (date_ptr fmt len) :: (date_ptr : _pointer) (fmt : _string) (size : _int = len) (buffer : _pointer = (malloc len 'atomic)) -> (result : _bool) -> (and result (cast buffer _pointer _string)))
(def-ocilib dategetdate OCI_DateGetDate : (date_ptr : _pointer) (year : (_ptr o _int)) (month : (_ptr o _int)) (day : (_ptr o _int)) -> (result : _bool) -> (and result (values year month day)))
(def-ocilib datesetdate OCI_DateSetDate : (date_ptr : _pointer) (year : _int) (month : _int) (day : _int) -> (result : _bool))
(def-ocilib dategetdatetime OCI_DateGetDateTime : (date_ptr : _pointer) (year : (_ptr o _int)) (month : (_ptr o _int)) (day : (_ptr o _int)) (hour : (_ptr o _int)) (min : (_ptr o _int)) (sec : (_ptr o _int)) -> (result : _bool) -> (and result (values year month day hour min sec)))
(def-ocilib datesetdatetime OCI_DateSetDateTime : (date_ptr : _pointer) (year : _int) (month : _int) (day : _int) (hour : _int) (min : _int) (sec : _int) -> (result : _bool))
(def-ocilib sysdate OCI_DateSysDate : (date_ptr : _pointer) -> (result : _bool))
(def-ocilib timestampcreate OCI_TimestampCreate	: (conn_ptr : _pointer) (type : _ocilib-timestamp-type) -> (ts_ptr : _pointer))
(def-ocilib timestampfree OCI_TimestampFree : (ts_ptr : _pointer) -> (result : _bool))
(def-ocilib timestampgettype OCI_TimestampGetType : (ts_ptr : _pointer) -> (type : _ocilib-timestamp-type))	
(def-ocilib systimestamp OCI_TimestampSysTimestamp : (ts_ptr : _pointer) -> (result : _bool))
; allocate byte buffer with make-bytes, use ffi function to cast to scheme
(def-ocilib timestamptotext OCI_TimestampToText : (ts_ptr fmt len precision) :: (ts_ptr : _pointer) (fmt : _string) (size : _int = len) (buffer : _bytes = (make-bytes len)) (precision : _int) -> (result : _bool) -> (and result (bytes->string/utf-8 (scheme_make_byte_string buffer))))

