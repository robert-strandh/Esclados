(cl:in-package #:asdf-user)

(defsystem "esclados-buffer"
  :depends-on ("mcclim"
               "esclados-utilities")
  :serial t
  :components
  ((:file "packages")
   (:file "buffer")))
