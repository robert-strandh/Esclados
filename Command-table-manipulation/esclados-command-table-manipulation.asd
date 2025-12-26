(cl:in-package #:asdf-user)

(defsystem "esclados-command-table-manipulation"
  :depends-on ("mcclim")
  :serial t
  :components
  ((:file "packages")
   (:file "command-table-manipulation")))
