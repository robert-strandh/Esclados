(cl:in-package #:common-lisp-user)

(defpackage #:esclados-help
  (:use #:common-lisp)
  (:local-nicknames (#:cmd #:esclados-command-processor)
                    (#:tbl #:esclados-command-table-manipulation)
                    (#:mini #:esclados-minibuffer)
                    (#:utils #:esclados-utils))
  (:export
   #:help-table))
