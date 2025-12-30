(cl:in-package #:common-lisp-user)

(defpackage #:esclados-command-parser
  (:use #:common-lisp)
  (:export
   #:command-parser
   #:partial-command-parser
   #:*extended-command-prompt*))
