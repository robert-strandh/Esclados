(cl:in-package #:common-lisp-user)

(defpackage #:esclados-keyboard-macros
  (:use #:common-lisp)
  (:local-nicknames (#:tbl #:esclados-command-table-manipulation)
                    (#:cmd #:esclados-command-processor))
  (:export #:keyboard-macro-table))
