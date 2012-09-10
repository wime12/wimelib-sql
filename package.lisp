;;;; package.lisp

(defpackage #:wimelib-sql
  (:use #:cl)
  (:export #:*sql-output*
	   #:add-sql-op
	   #:remove-sql-op
	   #:define-special-op
	   #:undefine-special-op
	   #:raw-string
	   #:process-literal
	   #:escape-sql
	   #:sql*
	   #:sql
	   #:intersperse
	   #:enable-embed-reader-syntax
	   #:disable-embed-reader-syntax
	   #:enable-column-reader-syntax
	   #:disable-column-reader-syntax))

