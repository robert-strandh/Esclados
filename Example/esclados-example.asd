(cl:in-package #:asdf-user)

(defsystem "esclados-example"
  :depends-on ("esclados")
  :serial t
  :components
  ((:file "packages")
   (:file "example-application")))
