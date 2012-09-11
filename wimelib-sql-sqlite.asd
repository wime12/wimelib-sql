;;;; wimelib-sql-sqlite.asd

(asdf:defsystem #:wimelib-sql-sqlite
  :serial t
  :description "A simple processor for the SQL language specialized on SQLite."
  :author "Wilfried Meindl <wilfried.meindl@gmail.com>"
  :license "BSD"
  :depends-on ("wimelib-sql")
  :components ((:file "sqlite")))

