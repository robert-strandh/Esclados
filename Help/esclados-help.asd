(cl:in-package #:asdf-user)

(defsystem "esclados-help"
  :depends-on ("mcclim"
               "esclados-utilities"
               "esclados-minibuffer"
               "esclados-command-processing"
               "esclados-command-table-manipulation"
               "esclados-frame")
  :serial t
  :components
  ((:file "packages")
   (:file "help")
   (:file "help-commands")))
