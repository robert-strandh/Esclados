(cl:in-package #:asdf-user)

(defsystem "esclados-example"
  :depends-on ("esclados"
               "esclados-info-pane"
               "esclados-command-processing")
  :serial t
  :components
  ((:file "packages")
   (:file "buffer")
   (:file "example-application")
   (:file "commands-and-key-bindings")))
