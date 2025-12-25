(cl:in-package #:asdf-user)

(defsystem "esclados-info-pane"
  :depends-on ("mcclim")
  :serial t
  :components
  ((:file "packages")
   (:file "info-pane")))
