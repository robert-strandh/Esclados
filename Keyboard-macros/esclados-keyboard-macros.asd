(cl:in-package #:asdf-user)

(defsystem "esclados-keyboard-macros"
  :depends-on ("mcclim"
               "esclados-command-table-manipulation"
               "esclados-command-processing")
  :serial t
  :components
  ((:file "packages")
   (:file "keyboard-macros")))
