(cl:in-package #:asdf-user)

(defsystem "esclados-pane"
  :depends-on ("mcclim")
  :serial t
  :components
  ((:file "packages")
   (:file "pane-mixin")))
