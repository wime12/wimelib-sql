;;;; package.lisp

(defpackage #:wimelib-sql
  (:use #:cl)
  (:export #:*sql-output*
	   #:define-sql-op
	   #:undefine-sql-op
	   #:define-special-op
	   #:undefine-special-op
	   #:raw-string
	   #:process-literal
	   #:escape-sql
	   #:sql-interpreter
	   #:get-sql-interpreter
	   #:*sql-interpreter*
	   #:sql*
	   #:sql-compiler
	   #:get-sql-compiler
	   #:*sql-compiler*
	   #:sql
	   #:intersperse
	   #:enable-embed-reader-syntax
	   #:disable-embed-reader-syntax
	   #:enable-column-reader-syntax
	   #:disable-column-reader-syntax))

