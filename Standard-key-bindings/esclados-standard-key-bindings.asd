(cl:in-package #:asdf-user)

(defsystem "esclados-standard-key-bindings"
  :depends-on ("mcclim"
               "esclados-command-table-manipulation"
               "esclados-frame"
               "esclados-minibuffer")
  :serial t
  :components
  ((:file "packages")
   (:file "standard-key-bindings")))
