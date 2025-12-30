(cl:in-package #:asdf-user)

(defsystem "esclados-top-level"
  :depends-on ("mcclim"
               "esclados-utilities"
               "esclados-command-processing"
               "esclados-minibuffer"
               "esclados-frame"
               "esclados-command-parser")
  :serial t
  :components
  ((:file "packages")
   (:file "top-level")))
