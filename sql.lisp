(in-package #:wimelib-sql)

;;; SQL Output

(defvar *sql-output* *standard-output*
  "The stream where the SQL output goes.")

;;; SQL Ops

(defgeneric sql-op-p (processor op)
  (:method (processor op)
    (declare (ignorable processor op))
    nil))

(defmacro define-sql-op (op processor-class)
  "An SQL op is just an object, usually a keyword like :SELECT,
that starts an SQL statement."
  `(defmethod sql-op-p ((processor ,processor-class) (op (eql ,op)))
     (declare (ignorable op))
     t))

;;; SQL Processor

(defclass sql-processor () ()
  (:documentation "The base class for all SQL processors."))

(defgeneric raw-string (processor string)
  (:documentation "Ultimately handles the string that is produced from SQL sexps."))

;;; Special Ops

(defgeneric special-op-p (processor op)
  (:method (processor op)
    (declare (ignore processor op))
    nil))

(defgeneric process-special-op (processor sexp)
  (:method ((processor sql-processor) sexp)
    (proc-special-op processor (car sexp) (cdr sexp))))

(defgeneric proc-special-op (processor special-op args))

(defmacro define-special-op (op ((processor processor-class) args) &body body)
  `(progn
     (defmethod special-op-p ((,processor ,processor-class) (op (eql ,op)))
       t)
     (defmethod proc-special-op ((,processor ,processor-class) (op (eql ,op)) ,args)
       ,@body)))

;;; Process literal values

(defgeneric process-literal (processor literal)
  (:documentation "Transforms literal values (e.g. strings, numbers, keywords, symbols) to their corresponding SQL form.")
  (:method ((processor sql-processor) (string string))
    (raw-string processor "'")
    (raw-string processor
		(with-output-to-string (buffer)
		  (with-input-from-string (str string)
		    (do ((c (read-char str nil) (read-char str nil)))
			((not c))
		      (case c
			((#\') (write-sequence "''" buffer))
			(t (write-char c buffer)))))))
    (raw-string processor "'"))
  (:method ((processor sql-processor) (number float))
    (raw-string processor (format nil "~,,,,,,'EE" number)))
  (:method ((processor sql-processor) (number integer))
    (raw-string processor (princ-to-string number)))
  (:method ((processor sql-processor) (symbol symbol))
    (when symbol
      (if (keywordp symbol)
	  (raw-string processor (escape-identifier (symbol-name symbol)))
	  (intersperse processor "." (mapcar #'make-symbol
					     (split #\. (symbol-name symbol)))
		       :key (lambda (processor part)
			      (quote-identifier processor part)))))))

(defvar *sql-identifier-quote* "\""
  "The string that surrounds SQL identifiers. Set to NIL or the empty string
if you do not want quoted identifiers.")

(defun quote-identifier (processor symbol)
  "Surrounds STRING with double quotes."
  (when *sql-identifier-quote*
    (raw-string processor *sql-identifier-quote*))
  (raw-string processor (escape-identifier (symbol-name symbol)))
  (when *sql-identifier-quote*
    (raw-string processor *sql-identifier-quote*)))

(defun escape-identifier (string)
  (substitute #\_ #\- string))

;;; The SQL processor

(defgeneric process-sql (processor sexp)
  (:documentation "Transforms an SQL sexp. Called with the standard sql-interpreter
it outputs a string representing the SQL statement on *SQL-OUTPUT*.
Called with the standard sql-compiler it returns code which writes
to *SQL-OUTPUT*.")
  (:method ((processor sql-processor) sexp)
    (cond ((atom sexp) (process-literal processor sexp))
	  ((special-op-p processor (car sexp)) (process-special-op processor sexp))
	  ((sql-op-p processor (car sexp)) (process-sql-op processor sexp))
	  ((keywordp (car sexp)) (process-sql-function processor sexp))
	  ((consp sexp) (process-sql processor (cons :list sexp))))))

(defun process-sql-op (processor sexp)
  (intersperse processor " " sexp))

(defun process-sql-function (processor sexp)
  (process-sql processor (car sexp))
  (raw-string processor "(")
  (intersperse processor ", " (cdr sexp))
  (raw-string processor ")"))

(defun intersperse (processor separator words &key (key #'process-sql))
  "Intersperses a separator between the results of calling KEY on the list
members of WORDS. KEY takes a processor and an SQL sexp."
  (loop
     for s in words
     for rest on words
     do (funcall key processor s)
     while (cdr rest)
     do (raw-string processor separator)))

(define-special-op :dot ((processor sql-processor) args)
  (intersperse processor "." args))

(defun list-helper (processor args)
  (intersperse processor ", " args
	       :key (lambda (processor args)
		      (if (and (symbolp (car args)) (not (keywordp (car args))))
			  (intersperse processor " " args)
			  (process-sql processor args)))))

(define-special-op :row ((processor sql-processor) args)
  (cond ((consp (car args))
	 (list-helper processor args))
	(t (intersperse processor ", " args))))

(define-special-op :list ((processor sql-processor) args)
  (cond ((consp (car args))
	 (raw-string processor "(")
	 (list-helper processor args)
	 (raw-string processor ")"))
	(t (raw-string processor "(")
	   (intersperse processor ", " args)
	   (raw-string processor ")"))))

;;; Interpreter

(defvar *sql-interpreter* nil)

(defclass sql-interpreter-mixin () ())

(defgeneric interprete-sql (processor sexp)
  (:method ((processor sql-interpreter-mixin) sexp)
    (process-sql processor sexp)
    (raw-string processor ";")))

(defmethod raw-string ((processor sql-interpreter-mixin) string)
  (write-sequence string *sql-output*))

(define-special-op :embed ((processor sql-interpreter-mixin) args)
  (declare (ignorable args))
  (error "Cannot embed values in interpreted mode."))

;;; Compiler

(defvar *sql-compiler* nil)

(defclass sql-compiler-mixin ()
  ((ops :accessor sql-compiler-ops
	:initform (make-ops-buffer))))

(defun make-ops-buffer ()
  (make-array 10 :adjustable t :fill-pointer 0))

(defun push-op (op ops)
  (vector-push-extend op ops))

(defgeneric compile-sql (processor sexp)
  (:method ((processor sql-compiler-mixin) sexp)
    (process-sql processor sexp)
    (push-op '(:raw-string ";") (sql-compiler-ops processor))
    (setf (sql-compiler-ops processor)
	  (optimize-op-array (sql-compiler-ops processor)))
    (values (generate-code (sql-compiler-ops processor))
	    (some #'embed-op-p (sql-compiler-ops processor)))))

(defmethod raw-string ((sql-compiler sql-compiler-mixin) string)
  (push-op `(:raw-string ,string) (sql-compiler-ops sql-compiler)))

(define-special-op :embed ((processor sql-compiler-mixin) args)
  (embed-value processor (car args)))

(defgeneric embed-value (sql-compiler value)
  (:method ((sql-compiler sql-compiler-mixin) value)
    (push-op `(:embed-value ,value) (sql-compiler-ops sql-compiler))))

(defun embed-op-p (op)
  (eq (car op) :embed-value))

(defun optimize-op-array (ops)
  (let ((new-ops (make-ops-buffer)))
    (with-output-to-string (buf)
      (flet ((push-buf ()
	       (let ((str (get-output-stream-string buf)))
		 (when (not (string= str ""))
		   (push-op `(:raw-string ,str) new-ops)))))
	(loop
	   for op across ops
	   do (case (first op)
		((:raw-string) (write-sequence (second op) buf))
		(t (push-buf)
		   (push-op op new-ops))))
	(push-buf)))
    new-ops))

(defun generate-code (ops)
  `(progn
     ,@(loop
	  :for op across ops
	  :collect (process-op (first op) (second op)))
     nil))

(defgeneric process-op (op arg)
  (:method ((op (eql :raw-string)) arg)
    `(write-sequence ,arg *sql-output*))
  (:method ((op (eql :embed-value)) arg)
    `(process-sql *sql-interpreter* ,arg)))

(define-special-op :splice ((processor sql-processor) args)
  (intersperse processor " " args))

;;; Reader syntax

(defun enable-column-reader-syntax ()
  (set-macro-character #\[
    (lambda (stream char)
      (declare (ignorable char))
      (cons :row (read-delimited-list #\] stream t))))
  (set-syntax-from-char #\] #\)))

(defun disable-column-reader-syntax ()
  (set-macro-character #\[ nil)
  (set-macro-character #\] nil))

(defun enable-embed-reader-syntax ()
  (set-macro-character #\@
    (lambda (stream char)
      (declare (ignorable char))
      (list :embed (read stream)))))

(defun disable-embed-reader-syntax ()
  (set-macro-character #\@ nil))

;; Auxiliary functions

(defun split-at (separator sequence)
  (let ((pos (position separator sequence)))
    (if pos
	(values
	 (subseq sequence 0 pos)
	 (subseq sequence (1+ pos)))
	(values sequence nil))))

(defun split (separator sequence)
  (loop
     with part
     with rest = sequence
     do (multiple-value-setq (part rest) (split-at separator rest))
     collect part
     while rest))
