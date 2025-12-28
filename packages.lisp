(cl:in-package #:common-lisp-user)

(defpackage #:esclados
  (:use :clim-lisp :clim-extensions)
  (:local-nicknames (#:utils #:esclados-utils)
                    (#:mini #:esclados-minibuffer)
                    (#:cmd #:esclados-command-processor)
                    (#:tbl #:esclados-command-table-manipulation)
                    (#:frame #:esclados-frame))
  (:export #:command-processor #:instant-macro-execution-mixin
           #:asynchronous-command-processor #:command-loop-command-processor
           #:dead-key-merging-command-processor
           #:command-for-unbound-gestures
           #:*extended-command-prompt*
           #:define-top-level #:top-level #:simple-command-loop
           #:gesture-name
	   #:invoke-with-help-stream #:with-help-stream
           #:find-applicable-command-table
           #:command-parser
           #:partial-command-parser
           #:command-table

           ;; General commands
           #:global-table
           #:com-quit #:com-extended-command

           ;; Keyboard macro commands
           #:keyboard-macro-table #:keyboard-macro-menu-table
           #:com-start-macro #:com-end-macro
           #:com-call-last-macro))
