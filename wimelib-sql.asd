;;;; wimelib-sql.asd

(asdf:defsystem #:wimelib-sql
  :serial t
  :description "A simple processor for the SQL language."
  :author "Wilfried Meindl <wilfried.meindl@gmail.com>"
  :license "BSD"
  :depends-on ("wimelib-utilities")
  :components ((:file "package")
               (:file "sql")))

