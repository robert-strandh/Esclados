(cl:in-package #:asdf-user)

(defsystem "esclados-frame"
  :depends-on ("mcclim")
  :serial t
  :components
  ((:file "packages")
   (:file "query")
   (:file "frame-mixin")))
