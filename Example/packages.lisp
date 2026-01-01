(cl:in-package #:common-lisp-user)

(defpackage #:esclados-example
  (:use #:common-lisp)
  (:local-nicknames (#:mini #:esclados-minibuffer)
                    (#:info #:esclados-info-pane)
                    (#:cmd #:esclados-command-processor)
                    (#:tbl #:esclados-command-table-manipulation)
                    (#:frame #:esclados-frame)
                    (#:pane #:esclados-pane)
                    (#:top #:esclados-top-level)
                    (#:key #:esclados-standard-key-bindings)
                    (#:kbm #:esclados-keyboard-macros))
  (:export #:example))
