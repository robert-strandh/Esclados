(cl:in-package #:esclados)

(defvar *extended-command-prompt*
  "The prompt used when querying the user for an extended
command. This only applies when the ESCLADOS command parser is being
used.")

(defgeneric top-level
    (frame
     &key
       command-parser
       command-unparser
       partial-command-parser
       prompt))

(setf (documentation 'top-level 'function)
      (format nil "Run a top-level loop for `frame', reading gestures~@
                   and invoking the appropriate commands."))

(defmacro define-top-level
    ((frame
      command-parser
      command-unparser
      partial-command-parser
      prompt)
     &key bindings)
  `(defmethod top-level
       (,frame &key
                 (,command-parser 'command-parser)
                 ;; FIXME: maybe customize this?  Under what
                 ;; circumstances would it be used?  Maybe try
                 ;; turning the clim listener into an ESCLADOS?
                 (,command-unparser  'clim:command-line-command-unparser)
                 (,partial-command-parser 'esclados-partial-command-parser)
                 (,prompt "Extended Command: "))
     ,(let ((frame (utils:unlisted frame)))
        `(with-slots (windows) ,frame
           (let ((*standard-output* (car windows))
                 (*standard-input* (clim:frame-standard-input ,frame))
                 (*minibuffer* (minibuffer ,frame))
                 (*print-pretty* nil)
                 (clim:*abort-gestures* *abort-gestures*)
                 (clim:*command-parser* ,command-parser)
                 (clim:*command-unparser* ,command-unparser)
                 (clim:*partial-command-parser* ,partial-command-parser)
                 (*extended-command-prompt* ,prompt)
                 (clim:*pointer-documentation-output*
                   (clim:frame-pointer-documentation-output ,frame))
                 (*esclados-instance* ,frame))
             (unless (eq (clim:frame-state ,frame) :enabled)
               (clim:enable-frame ,frame))
             (clim:redisplay-frame-panes ,frame :force-p t)
             (loop
               do (restart-case
                      (handler-case
                          (let* ((*command-processor* ,frame)
                                 (command-table (find-applicable-command-table ,frame))
                                 ,@bindings)
                            ;; for presentation-to-command-translators,
                            ;; which are searched for in
                            ;; (frame-command-table *application-frame*)
                            (clim:redisplay-frame-pane ,frame (clim:frame-standard-input ,frame))
                            (setf (clim:frame-command-table ,frame) command-table)
                            (process-gestures-or-command ,frame))
                        (unbound-gesture-sequence (c)
                          (display-message "~A is not bound" (gesture-name (gestures c)))
                          (clim:redisplay-frame-panes ,frame))
                        (clim:abort-gesture (c)
                          (if (overriding-handler ,frame)
                              (let ((*command-processor* (overriding-handler ,frame)))
                                (process-gesture (overriding-handler ,frame)
                                                 (clim:abort-gesture-event c)))
                              (display-message "Quit"))
                          (clim:redisplay-frame-panes ,frame)))
                    (return-to-esclados ()
                      (setf (overriding-handler ,frame) nil)
                      (setf (remaining-keys ,frame) nil)))))))))

(define-top-level
    (frame command-parser command-unparser partial-command-parser prompt))

(defmacro simple-command-loop (command-table loop-condition
                               &optional end-clauses (abort-clauses '((signal 'clim:abort-gesture :event *current-gesture*))))
  `(progn (setf (overriding-handler *command-processor*)
                (make-instance 'command-loop-command-processor
                               :command-table ,command-table
                               :end-condition #'(lambda ()
                                                  (not ,loop-condition))
                               :super-command-processor *command-processor*
                               :end-function #'(lambda ()
                                                 ,@end-clauses)
                               :abort-function #'(lambda ()
                                                   ,@abort-clauses)))))
