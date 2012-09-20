;;;; package.lisp

(defpackage #:wimelib-sql
  (:use #:cl)
  (:export #:*sql-output*
	   #:*sql-identifier-quote*
	   #:define-sql-op
	   #:sql-op-p
	   #:define-special-op
	   #:special-op-p
	   #:raw-string
	   #:sql-processor
	   #:process-sql
	   #:process-literal
	   #:sql-interpreter-mixin
	   #:interprete-sql
	   #:*sql-interpreter*
	   #:sql-compiler-mixin
	   #:compile-sql
	   #:*sql-compiler*
	   #:intersperse
	   #:enable-embed-reader-syntax
	   #:disable-embed-reader-syntax
	   #:enable-column-reader-syntax
	   #:disable-column-reader-syntax))
