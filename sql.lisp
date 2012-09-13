(in-package #:wimelib-sql)

;;; SQL Output

(defvar *sql-output* *standard-output*
  "The stream where the SQL output goes.")

;;; SQL Ops

(defvar *sql-ops* '())

(defun sql-op-p (op)
  (member op *sql-ops*))

(defmacro define-sql-op (op)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew ,op *sql-ops*)))

(defmacro undefine-sql-op (op)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *sql-ops* (remove ,op *sql-ops*))))

;;; SQL Processor

(defclass sql-processor () ())

(defgeneric raw-string (processor string)
  (:method ((processor sql-processor) string)
    (write-sequence string *sql-output*))
  (:documentation "Outputs a bare string on the SQL output."))

;;; Special Ops

(defvar *special-ops* '(:dot :columns :list :with-columns :embed))

(defun special-op-p (op)
  (member op *special-ops*))

(defgeneric process-special-op (processor sexp)
  (:method ((processor sql-processor) sexp)
    (proc-special-op processor (car sexp) (cdr sexp))))

(defgeneric proc-special-op (processor special-op args)
  (:method ((processor sql-processor) (special-op (eql :dot)) args)
    (intersperse processor "." args))
  (:method ((processor sql-processor) (special-op (eql :columns)) args)
    (intersperse processor ", " args))
  (:method ((processor sql-processor) (special-op (eql :list)) args)
    (raw-string processor "(")
    (intersperse processor ", " args)
    (raw-string processor ")"))
  (:method ((processor sql-processor) (special-op (eql :with-columns)) args)
    (raw-string processor "(")
    (intersperse processor ", " args
	       :key (lambda (processor args)
		      (intersperse processor " " args)))
    (raw-string processor ")")))

(defmacro undefine-special-op (op)
  "Undefines a special op."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-case
	 (progn
	   (setf *special-ops* (remove ,op *special-ops*))
	   (remove-method #'proc-special-op
			  (find-method #'proc-special-op '()
				       (list t `(eql ,',op) t)))
	   t)
       (simple-error (se)
	 (declare (ignore se))
	 nil))))

(defmacro define-special-op (op (processor args) &body body)
  "Defines a special op, a kind of SQL macro."
  `(progn
     (defmethod proc-special-op (,processor (special-op (eql ,op)) ,args)
       ,@body)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (pushnew ,op *special-ops*))))

;;; Escape literal values

(defgeneric escape-sql (processor obj)
  (:method ((processor sql-processor) (string string))
    (raw-string processor
		(with-output-to-string (buffer)
		  (with-input-from-string (str string)
		    (do ((c (read-char str nil) (read-char str nil)))
			((not c))
		      (case c
			((#\') (write-sequence "''" buffer))
			(t (write-char c buffer))))))))
  (:method ((processor sql-processor) (number float))
    (raw-string processor (format nil "~,,,,,,'EE" number)))
  (:method ((processor sql-processor) (number integer))
    (raw-string processor (princ-to-string number)))
  (:method ((processor sql-processor) (symbol symbol))
    (raw-string processor (substitute #\_ #\- (symbol-name symbol)))))

;;; Process literal values

(defgeneric process-literal (processor literal)
  (:method ((processor sql-processor) (string string))
    (raw-string processor "'")
    (escape-sql processor string)
    (raw-string processor "'"))
  (:method ((processor sql-processor) (number number))
    (escape-sql processor number))
  (:method ((processor sql-processor) (symbol symbol))
    (if (keywordp symbol)
	(raw-string processor (string-upcase (symbol-name symbol)))
	(intersperse processor "." (mapcar #'make-symbol
				       (split #\. (symbol-name symbol)))
		 :key (lambda (processor part)
			(quote-identifier processor part))))))

;;; The SQL parser

(defgeneric process-sql (processor sexp)
  (:documentation
   "Writes an SQL statement built from SEXP to STREAM using PROCESSOR.")
  (:method ((processor sql-processor) sexp)
    (cond ((atom sexp) (process-literal processor sexp))
	((special-op-p (car sexp)) (process-special-op processor sexp))
	((sql-op-p (car sexp)) (process-sql-op processor sexp))
	((keywordp (car sexp)) (process-sql-function processor sexp))
	((consp sexp) (process-sql processor (cons :list sexp))))))

(defvar *sql-identifier-quote* "\"")

(defun quote-identifier (processor symbol)
  "Surrounds STRING with double quotes."
  (when *sql-identifier-quote*
    (raw-string processor *sql-identifier-quote*))
  (escape-sql processor symbol)
  (when *sql-identifier-quote*
    (raw-string processor *sql-identifier-quote*)))

(defun process-sql-op (processor sexp)
  (intersperse processor " " sexp)
  (raw-string processor ";"))

(defun process-sql-function (processor sexp)
  (escape-sql processor (car sexp))
  (raw-string processor "(")
  (intersperse processor ", " (cdr sexp))
  (raw-string processor ")"))

(defun intersperse (processor separator words &key (key #'process-sql))
  "Outputs a string on PROCESSOR in which the transformed elements of
the list WORDS are separated by separator."
  (loop
     for s in words
     for rest on words
     do (funcall key processor s)
     while (cdr rest)
     do (raw-string processor separator)))

;;; Interpreter

(defclass sql-interpreter (sql-processor) ())

(defvar *sql-interpreter* nil)

(defun get-sql-interpreter ()
  (or *sql-interpreter* (make-instance 'sql-interpreter)))

(defun sql* (sexp)
  "Writes an SQL statement built from SEXP to *SQL-OUTPUT*."
  (process-sql (get-sql-interpreter) sexp)
  nil)

;;; Compiler

(defclass sql-compiler (sql-processor)
  ((ops :accessor sql-compiler-ops
	:initform (make-ops-buffer))))

(defvar *sql-compiler* nil)

(defun get-sql-compiler ()
  (or *sql-compiler* (make-instance 'sql-compiler)))

(defun make-ops-buffer ()
  (make-array 10 :adjustable t :fill-pointer 0))

(defun push-op (op ops)
  (vector-push-extend op ops))

(defmethod raw-string ((sql-compiler sql-compiler) string)
  (push-op `(:raw-string ,string) (sql-compiler-ops sql-compiler)))

(defmethod proc-special-op ((processor sql-compiler) (special-op (eql :embed))
			    args)
  (embed-value processor (car args)))

(defgeneric embed-value (sql-compiler value)
  (:method ((sql-compiler sql-compiler) value)
    (push-op `(:embed-value ,value) (sql-compiler-ops sql-compiler))))

(defun embed-op-p (op)
  (eq (car op) :embed-value))

(defun sexp->ops (sexp)
  (let ((compiler (make-instance 'sql-compiler)))
    (process-sql compiler sexp)
    (sql-compiler-ops compiler)))

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
  (let ((code `(progn
		 ,@(loop
		      for op across ops
		      collect (process-op (first op) (second op)))
		 nil)))
    (if (some #'embed-op-p ops)
	`(let ((*sql-interpreter* (get-sql-interpreter)))
	   ,code)
	code)))

(defgeneric process-op (op arg)
  (:method ((op (eql :raw-string)) arg)
    `(write-sequence ,arg *sql-output*))
  (:method ((op (eql :embed-value)) arg)
    `(process-sql *sql-interpreter* ,arg)))

(defmacro sql (sexp)
  (generate-code (optimize-op-array (sexp->ops sexp))))

;;; Reader syntax

(defun enable-column-reader-syntax ()
  (set-macro-character #\[
    (lambda (stream char)
      (declare (ignorable char))
      (cons :columns (read-delimited-list #\] stream t))))
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
