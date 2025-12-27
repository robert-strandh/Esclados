(cl:in-package #:common-lisp-user)

(defpackage #:esclados-frame
  (:use #:common-lisp)
  (:export
   #:*esclados-instance*
   #:buffers
   #:present-buffer
   #:current-buffer
   #:windows
   #:present-window
   #:current-window
   #:*previous-command*
   #:frame-mixin
   #:find-applicable-command-table
   #:applicable-command-table))
