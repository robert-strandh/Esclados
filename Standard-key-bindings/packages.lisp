(cl:in-package #:common-lisp-user)

(defpackage #:esclados-standard-key-bindings
  (:use #:common-lisp)
  (:local-nicknames (#:tbl #:esclados-command-table-manipulation)
                    (#:frame #:esclados-frame)
                    (#:mini #:esclados-minibuffer))
  (:export
   #:global-table
   #:com-quit
   #:com-extended-command))
