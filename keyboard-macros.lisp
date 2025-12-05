(cl:in-package #:esclados)

(clim:define-command-table keyboard-macro-table)

(clim:define-command (com-start-kbd-macro
                 :name t
                 :command-table keyboard-macro-table)
    ()
  (setf (recordingp *command-processor*) t)
  (setf (recorded-keys *command-processor*) '()))

(set-key 'com-start-kbd-macro 'keyboard-macro-table '((#\x :control) #\())

(clim:define-command (com-end-kbd-macro
                 :name t
                 :command-table keyboard-macro-table)
    ()
  (setf (recordingp *command-processor*) nil)
  (setf (recorded-keys *command-processor*)
        ;; this won't work if the command was invoked in any old way
        (reverse (cddr (recorded-keys *command-processor*)))))

(set-key 'com-end-kbd-macro 'keyboard-macro-table '((#\x :control) #\)))

(clim:define-command (com-call-last-kbd-macro
                 :name t
                 :command-table keyboard-macro-table)
    ((count 'integer :prompt "How many times?" :default 1))
  (setf (remaining-keys *command-processor*)
        (loop repeat count append (recorded-keys *command-processor*)))
  (setf (executingp *command-processor*) t))

(set-key `(com-call-last-kbd-macro ,clim:*numeric-argument-marker*)
         'keyboard-macro-table '((#\x :control) #\e))

(utils:define-menu-table keyboard-macro-menu-table (keyboard-macro-table)
  'com-start-kbd-macro
  'com-end-kbd-macro
  `(com-call-last-kbd-macro ,clim:*unsupplied-argument-marker*))

(setf (documentation 'com-start-kbd-macro 'function)
      (format nil "Start recording keys to define a keyboard macro.~@
                   ~@
                   Use C-x ) to finish recording the macro,~@
                   and C-x e to run it."  ))

(setf (documentation 'com-end-kbd-macro 'function)
      (format nil "Finish recording keys that define a keyboard macro.~@
                   ~@
                   Use C-x ( to start recording a macro,~@
                   and C-x e to run it."))

(setf (documentation 'com-call-last-kbd-macro 'function)
      (format nil "Run the last keyboard macro that was defined.~@
                   ~@
                   Use C-x ( to start and C-x ) to finish recording~@
                   a keyboard macro."))
