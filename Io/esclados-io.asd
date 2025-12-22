(cl:in-package #:asdf-user)

(defsystem "esclados-io"
  :depends-on ("mcclim"
               "esclados-utilities"
               "esclados-buffer"
               "esclados-minibuffer"
               "esclados")
  :serial t
  :components
  ((:file "packages")
   (:file "io")))
