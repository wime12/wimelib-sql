;;;; package.lisp

(defpackage #:wimelib-sql
  (:use #:cl)
  (:export #:*sql-output*
	   #:*sql-identifier-quote*
	   #:define-sql-op
	   #:undefine-sql-op
	   #:define-special-op
	   #:undefine-special-op
	   #:raw-string
	   #:process-sql
	   #:process-literal
	   #:sql-interpreter
	   #:get-sql-interpreter
	   #:*sql-interpreter*
	   #:sql-compiler
	   #:get-sql-compiler
	   #:*sql-compiler*
	   #:intersperse
	   #:enable-embed-reader-syntax
	   #:disable-embed-reader-syntax
	   #:enable-column-reader-syntax
	   #:disable-column-reader-syntax))
