(cl:in-package #:asdf-user)

(defsystem "esclados-utilities"
  :depends-on ("mcclim")
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "define-menu-table")
   (:file "name-mixin")
   (:file "gesture-name")))
