(in-package #:wimelib-sql)

;;; SQL Output

(defvar *sql-output* *standard-output*
  "The stream where the SQL output goes.")

(defgeneric raw-string (processor string)
  (:method ((stream stream) string)
    (write-sequence string stream))
  (:documentation "Outputs a bare string on the processor."))

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

;;; Special Ops

(defvar *special-ops* '(:dot :cols :list :coldef :embed))

(defun special-op-p (op)
  (member op *special-ops*))

(defun process-special-op (processor sexp)
  (proc-special-op processor (car sexp) (cdr sexp)))

(defgeneric proc-special-op (processor special-op args)
  (:method (processor (special-op (eql :dot)) args)
    (intersperse processor "." args))
  (:method (processor (special-op (eql :cols)) args)
    (intersperse processor ", " args))
  (:method (processor (special-op (eql :list)) args)
    (raw-string processor "(")
    (intersperse processor ", " args)
    (raw-string processor ")"))
  (:method (processor (special-op (eql :coldef)) args)
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

;;; Interpreter

(defun sql* (sexp)
  "Writes an SQL statement built from SEXP to *SQL-OUTPUT*."
  (process-sql *sql-output* sexp)
  nil)

;;; Escape literal values

(defgeneric escape-sql (processor obj)
  (:method (processor (string string))
    (raw-string processor
		(with-output-to-string (buffer)
		  (with-input-from-string (str string)
		    (do ((c (read-char str nil) (read-char str nil)))
			((not c))
		      (case c
			((#\') (write-sequence "''" buffer))
			(t (write-char c buffer))))))))
  (:method (processor (number float))
    (raw-string processor (format nil "~,,,,,,'EE" number)))
  (:method (processor (number integer))
    (raw-string processor (princ-to-string number)))
  (:method (processor (symbol symbol))
    (raw-string processor (substitute #\_ #\- (symbol-name symbol)))))

;;; Process literal values

(defgeneric process-literal (processor literal)
  (:method (processor (string string))
    (raw-string processor "'")
    (escape-sql processor string)
    (raw-string processor "'"))
  (:method (processor (number number))
    (escape-sql processor number))
  (:method (processor (symbol symbol))
    (if (keywordp symbol)
	(raw-string processor (string-upcase (symbol-name symbol)))
	(intersperse processor "." (mapcar #'make-symbol
				       (split #\. (symbol-name symbol)))
		 :key (lambda (processor part)
			(quote-identifier processor part))))))

;;; The SQL parser

(defun process-sql (processor sexp)
  "Writes an SQL statement built from SEXP to STREAM using PROCESSOR."
  (cond ((atom sexp) (process-literal processor sexp))
	((special-op-p (car sexp)) (process-special-op processor sexp))
	((sql-op-p (car sexp)) (process-sql-op processor sexp))
	((keywordp (car sexp)) (process-sql-function processor sexp))
	((and (consp sexp) (consp (car sexp)))
	 (process-sql processor (cons :coldef sexp)))
	((consp sexp) (process-sql processor (cons :list sexp)))))

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

;;; Compiler

(defclass sql-compiler ()
  ((ops :accessor sql-compiler-ops
	:initform (make-ops-buffer))))

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
  `(progn
     ,@(loop
	  for op across ops
	  collect (process-op (first op) (second op)))
     nil))

(defgeneric process-op (op arg)
  (:method ((op (eql :raw-string)) arg)
    `(write-sequence ,arg *sql-output*))
  (:method ((op (eql :embed-value)) arg)
    `(sql* ,arg)))

(defmacro sql (sexp)
  (generate-code (optimize-op-array (sexp->ops sexp))))

;;; Reader for [] syntax

(defun enable-column-reader-syntax ()
  (set-macro-character #\[
    (lambda (stream char)
      (declare (ignorable char))
      (cons :cols (read-delimited-list #\] stream t))))
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
