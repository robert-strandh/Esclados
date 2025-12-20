(cl:in-package #:asdf-user)

(defsystem esclados-command-processing
  :depends-on ("mcclim"
               "esclados-minibuffer")
  :serial t
  :components
  ((:file "packages")
   (:file "command-processing")))

