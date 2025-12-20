(cl:in-package #:common-lisp-user)

(defpackage #:esclados-minibuffer
  (:use #:common-lisp)
  (:export
   #:minibuffer-pane
   #:*minibuffer*
   #:display-message
   #:with-minibuffer-stream))
