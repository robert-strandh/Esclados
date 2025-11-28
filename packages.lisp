(defpackage :esclados-utils
  (:use :clim-lisp :c2mop :clim)
  (:shadowing-import-from :clim-lisp #:describe-object)
  (:shadowing-import-from :c2mop
                          #:defclass
                          #:defgeneric
                          #:defmethod
                          #:standard-generic-function
                          #:standard-method
                          #:standard-class)
  (:export #:unlisted
           #:listed
           #:list-aref
           #:build-menu #:define-menu-table
           #:name-mixin #:name
           #:mode))

(defpackage :esclados
  (:use :clim-lisp :clim :esclados-utils :clim-extensions)
  (:export #:*esclados-instance*
           #:buffers #:esclados-current-buffer #:current-buffer
           #:windows #:esclados-current-window #:current-window
           #:*previous-command*
           #:*minibuffer* #:minibuffer #:minibuffer-pane #:display-message
           #:with-minibuffer-stream
           #:esclados-pane-mixin #:previous-command
           #:info-pane #:master-pane
           #:esclados-frame-mixin #:recordingp #:executingp
           #:*esclados-abort-gestures* #:*current-gesture* #:*command-processor*
           #:unbound-gesture-sequence #:gestures
           #:command-processor #:instant-macro-execution-mixin
           #:asynchronous-command-processor #:command-loop-command-processor
           #:dead-key-merging-command-processor
           #:overriding-handler #:directly-processing-p #:process-gesture #:process-gestures-or-command
           #:command-for-unbound-gestures
           #:*extended-command-prompt*
           #:define-esclados-top-level #:esclados-top-level #:simple-command-loop
           #:convert-to-gesture #:gesture-name
	   #:invoke-with-help-stream #:with-help-stream
           #:set-key
           #:find-applicable-command-table
           #:esclados-command-parser
           #:esclados-partial-command-parser
           #:esclados-command-table

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

(defpackage :esclados-buffer
  (:use :clim-lisp :clim :esclados :esclados-utils)
  (:export #:frame-make-buffer-from-stream #:make-buffer-from-stream
           #:frame-save-buffer-to-stream #:save-buffer-to-stream
           #:filepath #:name #:needs-saving #:file-write-time #:file-saved-p
           #:esclados-buffer-mixin
           #:frame-make-new-buffer #:make-new-buffer
           #:read-only-p))

(defpackage :esclados-io
  (:use :clim-lisp :clim :esclados :esclados-buffer :esclados-utils)
  (:export #:frame-find-file #:find-file
           #:frame-find-file-read-only #:find-file-read-only
           #:frame-set-visited-file-name #:set-visited-filename
           #:check-buffer-writability
           #:frame-save-buffer #:save-buffer
           #:frame-write-buffer #:write-buffer
           #:buffer-writing-error #:buffer #:filepath
           #:filepath-is-directory
           #:io-table #:esclados-io-menu-table
           #:com-find-file #:com-find-file-read-only
           #:com-read-only #:com-set-visited-file-name
           #:com-save-buffer #:com-write-buffer))
