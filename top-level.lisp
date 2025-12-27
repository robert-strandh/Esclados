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
                 (,partial-command-parser 'partial-command-parser)
                 (,prompt "Extended Command: "))
     ,(let ((frame (utils:unlisted frame)))
        `(with-accessors ((windows windows)) ,frame
           (let ((*standard-output* (car windows))
                 (*standard-input* (clim:frame-standard-input ,frame))
                 (mini:*minibuffer* (minibuffer ,frame))
                 (*print-pretty* nil)
                 (clim:*abort-gestures* cmd:*abort-gestures*)
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
                          (let* ((cmd:*command-processor* ,frame)
                                 (command-table applicable-command-table)
                                 ,@bindings)
                            ;; for presentation-to-command-translators,
                            ;; which are searched for in
                            ;; (frame-command-table *application-frame*)
                            (clim:redisplay-frame-pane ,frame (clim:frame-standard-input ,frame))
                            (setf (clim:frame-command-table ,frame) command-table)
                            (cmd:process-gestures-or-command ,frame))
                        (cmd:unbound-gesture-sequence (c)
                          (mini:display-message
                           "~A is not bound"
                           (utils:gesture-name (cmd:gestures c)))
                          (clim:redisplay-frame-panes ,frame))
                        (clim:abort-gesture (c)
                          (if (cmd:overriding-handler ,frame)
                              (let ((cmd:*command-processor* (cmd:overriding-handler ,frame)))
                                (cmd:process-gesture (cmd:overriding-handler ,frame)
                                                 (clim:abort-gesture-event c)))
                              (mini:display-message "Quit"))
                          (clim:redisplay-frame-panes ,frame)))
                    (return-to-esclados ()
                      (setf (cmd:overriding-handler ,frame) nil)
                      (setf (cmd:remaining-keys ,frame) nil)))))))))

(define-top-level
    (frame command-parser command-unparser partial-command-parser prompt))

(defmacro simple-command-loop (command-table loop-condition
                               &optional end-clauses (abort-clauses '((signal 'clim:abort-gesture :event *current-gesture*))))
  `(progn (setf (cmd:overriding-handler cmd:*command-processor*)
                (make-instance 'cmd:command-loop-command-processor
                  :command-table ,command-table
                  :end-condition #'(lambda ()
                                     (not ,loop-condition))
                  :super-command-processor cmd:*command-processor*
                  :end-function #'(lambda ()
                                    ,@end-clauses)
                  :abort-function #'(lambda ()
                                      ,@abort-clauses)))))
