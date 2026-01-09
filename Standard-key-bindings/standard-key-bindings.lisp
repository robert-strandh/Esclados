(cl:in-package #:esclados-standard-key-bindings)

(clim:define-command-table global-table)

(clim:define-command (com-quit :name t :command-table global-table) ()
  (clim:frame-exit clim:*application-frame*))

(setf (documentation 'com-quit 'function)
      (format nil "Exit.~@
                   ~@
                   First ask if modified buffers should be saved.~@
                   If you decide not to save a modified buffer, you~@
                   will be asked to confirm your decision to exit."))

(tbl:set-key 'com-quit 'global-table '((#\x :control) (#\c :control)))

(clim:define-command (com-extended-command :command-table global-table)
    ()
  (let* ((command-table frame:applicable-command-table)
         (item (handler-case
                   (clim:accept
                    `(clim:command :command-table ,command-table)
                    ;; this gets erased immediately anyway
                    :prompt "" :prompt-mode :raw)
                 ((or clim:command-not-accessible
                      clim:command-not-present)
                   ()
                  (clim:beep)
                  (mini:display-message "No such command")
                  (return-from com-extended-command nil)))))
    (clim:execute-frame-command clim:*application-frame* item)))

(setf (documentation 'com-extended-command 'function)
      (format nil "Prompt for a command name and arguments, then run it."))

(tbl:set-key 'com-extended-command 'global-table '((#\x :meta)))
