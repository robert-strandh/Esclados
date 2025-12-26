(cl:in-package #:asdf-user)

(defsystem "esclados-event-handling"
  :depends-on ("mcclim")
  :serial t
  :components
  ((:file "packages")
   (:file "event-handling")))
