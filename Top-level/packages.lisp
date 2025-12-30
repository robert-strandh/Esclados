(cl:in-package #:common-lisp-user)

(defpackage #:esclados-top-level
  (:use #:common-lisp)
  (:local-nicknames (#:utils #:esclados-utils)
                    (#:cmd #:esclados-command-processor)
                    (#:mini #:esclados-minibuffer)
                    (#:frame #:esclados-frame)
                    (#:cmp #:esclados-command-parser))
  (:export
   #:*extended-command-prompt*
   #:top-level))
