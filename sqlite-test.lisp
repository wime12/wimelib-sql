(in-package #:wimelib-sql)

(defvar *db*)

(defmacro ssql (sexp)
  `(with-output-to-string (*sql-output*)
     (sql ,sexp)))

(defun open-db (name)
  (setf *db* (wimelib-sqlite3:sqlite3-open name)))

(defmacro query (sexp)
  `(let ((result nil))
     (wimelib-sqlite3:sqlite3-exec *db* (ssql ,sexp)
				   (lambda (res)
				     (when (not result)
				       (push (wimelib-sqlite3:sqlite3-column-names
					      res) result))
				     (push (wimelib-sqlite3:sqlite3-row-values res)
					   result)))
     (let ((res (nreverse result)))
       (values (cdr res)
	       (car res)))))

(defmacro with-transaction (&body body)
  `(progn
     (wimelib-sqlite3:sqlite3-exec *db* (ssql (:begin :transaction)))
     ,@body
     (wimelib-sqlite3:sqlite3-exec *db* (ssql (:commit :transaction)))))
