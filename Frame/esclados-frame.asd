(cl:in-package #:asdf-user)

(defsystem "esclados-frame"
  :depends-on ("mcclim"
               "esclados-command-processing"
               "esclados-pane")
  :serial t
  :components
  ((:file "packages")
   (:file "query")
   (:file "frame-mixin")))
