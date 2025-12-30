(cl:in-package #:asdf-user)

(defsystem "esclados-command-parser"
  :depends-on ("mcclim")
  :serial t
  :components
  ((:file "packages")
   (:file "command-parser")))
