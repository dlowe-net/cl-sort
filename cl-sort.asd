;;;; cl-sort.asd

(asdf:defsystem #:cl-sort
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :description "Collection of sort algorithms"
  :serial t
  :components ((:file "package")
               (:file "cl-sort")))

(asdf:defsystem #:cl-sort.test
  :author "Daniel Lowe <dlowe@dlowe.net>"
  :description "Tests for the collection of sort algorithms"
  :depends-on (#:cl-sort)
  :serial t
  :components ((:file "tests")))

(defmethod asdf:perform ((op test-op) (system (eql (find-system '#:cl-sort))))
  (asdf:load-system '#:cl-sort.test)
  (eval (read-from-string "(cl-sort.tests:run-tests)")))
