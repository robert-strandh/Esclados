(cl:in-package #:asdf-user)

(defsystem esclados-minibuffer
  :depends-on ("mcclim")
  :serial t
  :components
  ((:file "packages")
   (:file "minibuffer-pane")))
