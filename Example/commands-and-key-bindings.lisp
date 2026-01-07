(cl:in-package #:esclados-example)

(clim:define-command-table global-example-table
  :inherit-from (key:global-table kbm:keyboard-macro-table help:help-table))

(clim:define-command
    (com-insert-character
     :name t
     :command-table global-example-table)
    ((character 'character))
  (insert-character character (buffer clim:*application-frame*)))

(clim:define-command
    (com-delete-character
     :name t
     :command-table global-example-table)
    ()
  (delete-character (buffer clim:*application-frame*)))

(clim:define-command
    (com-forward-character
     :name t
     :command-table global-example-table)
    ()
  (forward-character (buffer clim:*application-frame*)))

(clim:define-command
    (com-backward-character
     :name t
     :command-table global-example-table)
    ()
  (backward-character (buffer clim:*application-frame*)))

(loop for character across " !\"#$%&'()*+,-./"
      do (tbl:set-key `(com-insert-character ,character)
                      'global-example-table
                      `(,character)))

(loop for character across "0123456789:;<=>?"
      do (tbl:set-key `(com-insert-character ,character)
                      'global-example-table
                      `(,character)))

(loop for character across "@ABCDEFGHIJKLMNO"
      do (tbl:set-key `(com-insert-character ,character)
                      'global-example-table
                      `(,character)))

(loop for character across "PQRSTUVWXYZ[\\]^_"
      do (tbl:set-key `(com-insert-character ,character)
                      'global-example-table
                      `(,character)))

(loop for character across "`abcdefghijklmno"
      do (tbl:set-key `(com-insert-character ,character)
                      'global-example-table
                      `(,character)))

(loop for character across "pqrstuvwxyz{|}~"
      do (tbl:set-key `(com-insert-character ,character)
                      'global-example-table
                      `(,character)))

(tbl:set-key `(com-insert-character #\Newline)
             'global-example-table
             `(#\Return))

(tbl:set-key 'com-delete-character
             'global-example-table
             `((#\d :control)))

(tbl:set-key 'com-forward-character
             'global-example-table
             `((#\f :control)))

(tbl:set-key 'com-backward-character
             'global-example-table
             `((#\b :control)))

(defmethod clim:execute-frame-command :around ((frame example) command)
  (declare (ignore command))
  (handler-case (call-next-method)
    (beginning-of-buffer ()
      (mini:display-message "Beginning of buffer"))
    (end-of-buffer ()
      (mini:display-message "End of buffer"))))
