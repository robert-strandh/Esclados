(cl:in-package #:common-lisp-user)

(defpackage #:esclados
  (:use :clim-lisp :clim-extensions)
  (:local-nicknames (#:utils #:esclados-utils)
                    (#:mini #:esclados-minibuffer)
                    (#:cmd #:esclados-command-processor)
                    (#:tbl #:esclados-command-table-manipulation))
  (:export #:*esclados-instance*
           #:buffers #:present-buffer #:current-buffer
           #:windows #:present-window #:current-window
           #:*previous-command*
           #:*minibuffer* #:minibuffer #:minibuffer-pane #:display-message
           #:with-minibuffer-stream
           #:pane-mixin #:previous-command
           #:info-pane #:master-pane
           #:frame-mixin #:recordingp #:executingp
           #:*abort-gestures* #:*current-gesture* #:*command-processor*
           #:unbound-gesture-sequence #:gestures
           #:command-processor #:instant-macro-execution-mixin
           #:asynchronous-command-processor #:command-loop-command-processor
           #:dead-key-merging-command-processor
           #:overriding-handler #:directly-processing-p #:process-gesture #:process-gestures-or-command
           #:command-for-unbound-gestures
           #:*extended-command-prompt*
           #:define-top-level #:top-level #:simple-command-loop
           #:gesture-name
	   #:invoke-with-help-stream #:with-help-stream
           #:find-applicable-command-table
           #:command-parser
           #:partial-command-parser
           #:command-table

           #:gesture-matches-gesture-name-p #:meta-digit
           #:proper-gesture-p
           #:universal-argument #:meta-minus

           ;; General commands
           #:global-table
           #:com-quit #:com-extended-command

           ;; Help commands
           #:help-table #:help-menu-table
           #:com-describe-key-briefly #:com-where-is
           #:com-describe-bindings
           #:com-describe-key #:com-describe-command
           #:com-apropos-command

           ;; Keyboard macro commands
           #:keyboard-macro-table #:keyboard-macro-menu-table
           #:com-start-macro #:com-end-macro
           #:com-call-last-macro))
