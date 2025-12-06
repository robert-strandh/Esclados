(in-package #:esclados)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ESCLADOS frame mixin

(defclass esclados-frame-mixin (command-processor)
  ((windows :accessor windows)))

(defmethod present-buffer ((esclados esclados-frame-mixin))
  (first (buffers esclados)))

(defmethod present-window ((esclados esclados-frame-mixin))
  (first (windows esclados)))

(defmethod esclados-command-table ((frame esclados-frame-mixin))
  (find-applicable-command-table frame))

;; Defaults for non-ESCLADOS-frames.
(defmethod recordingp ((frame clim:application-frame))
  nil)

(defmethod executingp ((frame clim:application-frame))
  nil)

(defmethod recorded-keys ((frame clim:application-frame))
  nil)

(defmethod remaining-keys ((frame clim:application-frame))
  nil)

(defmethod minibuffer ((clim:application-frame esclados-frame-mixin))
  (clim:frame-standard-input clim:application-frame))

(defmethod clim:redisplay-frame-panes :around ((frame esclados-frame-mixin) &key force-p)
  (declare (ignore force-p))
  (when (null (remaining-keys frame))
    (setf (executingp frame) nil)
    (call-next-method)))

(defmethod clim:execute-frame-command :after ((frame esclados-frame-mixin) command)
  ;; FIXME: I'm not sure that we want to do this for commands sent
  ;; from other threads; we almost certainly don't want to do it twice
  ;; in such cases...
  (setf (previous-command (present-window frame)) command))

(defmethod clim:execute-frame-command :around ((frame esclados-frame-mixin) command)
  (declare (ignore command))
  (call-next-method)
  (when (eq frame clim:*application-frame*)
    (clim:redisplay-frame-panes frame)))

(defgeneric find-applicable-command-table (frame)
  (:documentation "Return the command table object that commands
on `frame' should be found in."))

(defmethod find-applicable-command-table ((frame esclados-frame-mixin))
  (esclados-command-table (car (windows frame))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Top level

(defvar *extended-command-prompt*
  "The prompt used when querying the user for an extended
command. This only applies when the ESCLADOS command parser is being
used.")

(defgeneric esclados-top-level (frame &key
                                   command-parser
                                   command-unparser
                                   partial-command-parser
                                   prompt)
  (:documentation "Run a top-level loop for `frame', reading
  gestures and invoking the appropriate commands."))

(defmacro define-esclados-top-level ((frame command-parser
                                 command-unparser
                                 partial-command-parser
                                 prompt) &key bindings)
  `(defmethod esclados-top-level (,frame &key
                                      (,command-parser 'esclados-command-parser)
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
                 (clim:*abort-gestures* *esclados-abort-gestures*)
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

(define-esclados-top-level (frame command-parser
                             command-unparser
                             partial-command-parser
                             prompt))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Event handling.

(defgeneric convert-to-gesture (clim:event)
  (:documentation "Convert `event' (which must be an input event)
  to a CLIM gesture, or NIL, if this is not possible."))

(defmethod convert-to-gesture ((ev clim:event))
  nil)

(defmethod convert-to-gesture ((ev character))
  ev)

(defmethod convert-to-gesture ((ev symbol))
  ev)

;;; This is dubious. -- jd 2022-12-22
(defmethod convert-to-gesture ((clim:event clim:key-press-event))
  (let ((modifiers (clim:event-modifier-state clim:event)))
    (when (or (zerop modifiers)
              (eql modifiers clim:+shift-key+))
      (let ((char (clim:keyboard-event-character clim:event)))
        (if (eql char #\return)
            #\newline
            (or char clim:event))))))

(defmethod convert-to-gesture ((ev clim:pointer-button-press-event))
  ev)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; command table manipulation

;;; Helper to avoid calling find-keystroke-item at load time. In Classic CLIM
;;; that function doesn't work if not connected to a port.

(defun compare-gestures (g1 g2)
  (and (eql (car g1) (car g2))
       (eql (apply #'clim:make-modifier-state (cdr g1))
            (apply #'clim:make-modifier-state (cdr g2)))))

(defun find-gesture-item (table gesture)
  (clim:map-over-command-table-keystrokes
   (lambda (name gest item)
     (declare (ignore name))
     (when (compare-gestures gesture gest)
       (return-from find-gesture-item item)))
   table)
  nil)

(defun ensure-subtable (table gesture)
  ;; Not having a sheet here is not conforming.
  (let* ((modifier-state (apply #'clim:make-modifier-state (cdr gesture)))
         (clim:event (make-instance 'clim:key-press-event
                               :sheet nil
                               :x 0 :y 0
                               :key-name nil
                               :key-character (car gesture)
                               :modifier-state modifier-state))
         (item (clim:find-keystroke-item clim:event table :errorp nil)))
    (when (or (null item) (not (eq (clim:command-menu-item-type item) :menu)))
      (let ((name (gensym)))
        (clim:make-command-table name :errorp nil)
        (clim:add-menu-item-to-command-table table (symbol-name name)
                                        :menu name
                                        :keystroke gesture)))
    (clim:command-menu-item-value
     (clim:find-keystroke-item clim:event table :errorp nil))))

(defun set-key (clim:command table gestures)
  (unless (consp clim:command)
    (setf clim:command (list clim:command)))
  (let ((gesture (car gestures)))
    (cond ((null (cdr gestures))
           (clim:add-keystroke-to-command-table
            table gesture :command clim:command :errorp nil)
           (when (and (listp gesture)
                      (find :meta gesture))
             ;; KLUDGE: this is a workaround for poor McCLIM
             ;; behaviour; really this canonization should happen in
             ;; McCLIM's input layer.
             (set-key clim:command table
                      (list (list :escape)
                            (let ((esc-list (remove :meta gesture)))
                              (if (and (= (length esc-list) 2)
                                       (find :shift esc-list))
                                  (remove :shift esc-list)
                                  esc-list))))))
          (t (set-key clim:command
                      (ensure-subtable table gesture)
                      (cdr gestures))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; standard key bindings

;;; global

(clim:define-command-table global-table)

(clim:define-command (com-quit :name t :command-table global-table) ()
  "Exit.
First ask if modified buffers should be saved. If you decide not to save a modified buffer, you will be asked to confirm your decision to exit."
  (clim:frame-exit clim:*application-frame*))

(set-key 'com-quit 'global-table '((#\x :control) (#\c :control)))

(clim:define-command (com-extended-command
                 :command-table global-table)
    ()
  "Prompt for a command name and arguments, then run it."
  (let ((item (handler-case
                  (clim:accept
                   `(clim:command :command-table ,(find-applicable-command-table clim:*application-frame*))
                   ;; this gets erased immediately anyway
                   :prompt "" :prompt-mode :raw)
                ((or clim:command-not-accessible clim:command-not-present) ()
                  (clim:beep)
                  (display-message "No such command")
                  (return-from com-extended-command nil)))))
    (clim:execute-frame-command clim:*application-frame* item)))

(set-key 'com-extended-command 'global-table '((#\x :meta)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Help

(defgeneric invoke-with-help-stream (esclados title continuation)
  (:documentation
   #.(format nil "Invoke CONTINUATION with a single argument - a stream~@
                  for writing on-line help for ESCLADOS onto. The stream~@
                  should have the title, or name, TITLE (a string), but the~@
                  specific meaning of this is left to the respective ESCLADOS.")))

(defmethod invoke-with-help-stream (frame title continuation)
  (declare (ignore frame))
  (funcall continuation
           (clim:open-window-stream
            :label title
            :input-buffer (climi::frame-event-queue clim:*application-frame*)
            :width 400)))

(defmacro with-help-stream ((stream title) &body body)
  "Evaluate `body' with `stream' bound to a stream suitable for
writing help information on. `Title' must evaluate to a string,
and will be used for naming the resulting stream, if that makes
sense for the ESCLADOS."
  `(invoke-with-help-stream *esclados-instance* ,title
                            #'(lambda (,stream)
                                ,@body)))

(defun read-gestures-for-help (clim:command-table)
  (clim:with-input-focus (t)
    (loop for gestures = (list (esclados-read-gesture))
            then (nconc gestures (list (esclados-read-gesture)))
          for item = (find-gestures-with-inheritance gestures clim:command-table)
          unless item
            do (return (values nil gestures))
          when (eq (clim:command-menu-item-type item) :command)
            do (return (values (clim:command-menu-item-value item) gestures)))))

(defun describe-key-briefly (frame)
  (let ((clim:command-table (find-applicable-command-table frame)))
    (multiple-value-bind (clim:command gestures)
        (read-gestures-for-help clim:command-table)
      (when (consp clim:command)
        (setf clim:command (car clim:command)))
      (display-message "~{~A ~}~:[is not bound~;runs the command ~:*~A~]"
                       (mapcar #'gesture-name gestures)
                       (or (clim:command-line-name-for-command
                            clim:command clim:command-table :errorp nil)
                           clim:command)))))

(defgeneric gesture-name (gesture))

(defmethod gesture-name ((char character))
  (if (and (graphic-char-p char)
           (not (char= char #\Space)))
      (string char)
      (or (char-name char)
          char)))

(defun translate-name-and-modifiers (key-name modifiers)
  (with-output-to-string (s)
    (loop for (modifier name) on (list
                                        ;(+alt-key+ "A-")
                                  clim:+hyper-key+ "H-"
                                  clim:+super-key+ "s-"
                                  clim:+meta-key+ "M-"
                                  clim:+control-key+ "C-")
          by #'cddr
          when (plusp (logand modifier modifiers))
            do (princ name s))
    (princ (if (typep key-name 'character)
               (gesture-name key-name)
               key-name) s)))

(defmethod gesture-name ((ev clim:keyboard-event))
  (let ((key-name (clim:keyboard-event-key-name ev))
        (modifiers (clim:event-modifier-state ev)))
    (translate-name-and-modifiers key-name modifiers)))

(defmethod gesture-name ((gesture list))
  (cond ((eq (car gesture) :keyboard)
         (translate-name-and-modifiers (second gesture) (third gesture)))
        ;; Assume `gesture' is a list of gestures.
        (t (format nil "~{~A~#[~; ~; ~]~}" (mapcar #'gesture-name gesture)))))

(defun find-keystrokes-for-command (clim:command clim:command-table)
  (let ((keystrokes '()))
    (labels ((helper (clim:command clim:command-table prefix)
               (clim:map-over-command-table-keystrokes
                #'(lambda (menu-name keystroke item)
                    (declare (ignore menu-name))
                    (cond ((and (eq (clim:command-menu-item-type item) :command)
                                (or (and (symbolp (clim:command-menu-item-value item))
                                         (eq (clim:command-menu-item-value item) clim:command))
                                    (and (listp (clim:command-menu-item-value item))
                                         (eq (car (clim:command-menu-item-value item)) clim:command))))
                           (push (cons keystroke prefix) keystrokes))
                          ((eq (clim:command-menu-item-type item) :menu)
                           (helper clim:command (clim:command-menu-item-value item) (cons keystroke prefix)))
                          (t nil)))
                clim:command-table)))
      (helper clim:command clim:command-table nil)
      keystrokes)))

(defun find-keystrokes-for-command-with-inheritance (clim:command start-table)
  (let ((keystrokes '()))
    (labels  ((helper (table)
                (let ((keys (find-keystrokes-for-command clim:command table)))
                  (when keys (push keys keystrokes))
                  (dolist (subtable (clim:command-table-inherit-from
                                     (clim:find-command-table table)))
                    (helper subtable)))))
      (helper start-table))
    keystrokes))

(defun find-all-keystrokes-and-commands (clim:command-table)
  (let ((results '()))
    (labels ((helper (clim:command-table prefix)
               (clim:map-over-command-table-keystrokes
                #'(lambda (menu-name keystroke item)
                    (declare (ignore menu-name))
                    (cond ((eq (clim:command-menu-item-type item) :command)
                           (push (cons (cons keystroke prefix)
                                       (clim:command-menu-item-value item))
                                 results))
                          ((eq (clim:command-menu-item-type item) :menu)
                           (helper (clim:command-menu-item-value item) (cons keystroke prefix)))
                          (t nil)))
                clim:command-table)))
      (helper clim:command-table nil)
      results)))

(defun find-all-keystrokes-and-commands-with-inheritance (start-table)
  (let ((results '()))
    (labels  ((helper (table)
                (let ((res (find-all-keystrokes-and-commands table)))
                  (when res  (setf results (nconc res results)))
                  (dolist (subtable (clim:command-table-inherit-from
                                     (clim:find-command-table table)))
                    (helper subtable)))))
      (helper start-table))
    results))

(defun find-all-commands-and-keystrokes-with-inheritance (start-table)
  (let ((results '()))
    (clim:map-over-command-table-commands
     (lambda (clim:command)
       (let ((keys (find-keystrokes-for-command-with-inheritance clim:command start-table)))
         (push (cons clim:command keys) results)))
     start-table
     :inherited t)
    results))

(defun sort-by-name (list)
  (sort list #'string< :key (lambda (item)
                              (symbol-name (if (listp (cdr item))
                                               (cadr item)
                                               (cdr item))))))

(defun sort-by-keystrokes (list)
  (sort list (lambda (a b)
               (cond ((and (characterp a)
                           (characterp b))
                      (char< a b))
                     ((characterp a)
                      t)
                     ((characterp b)
                      nil)
                     (t (string< (symbol-name a)
                                 (symbol-name b)))))
        :key (lambda (item) (second (first (first item))))))

(defun describe-bindings (stream clim:command-table
                          &optional (sort-function #'sort-by-name))
  (clim:formatting-table (stream)
    (loop for (keys . clim:command)
            in (funcall sort-function
                        (find-all-keystrokes-and-commands-with-inheritance
                         clim:command-table))
          when (consp clim:command) do (setq clim:command (car clim:command))
            do (clim:formatting-row (stream)
                 (clim:formatting-cell (stream :align-x :right)
                   (clim:with-text-style (stream '(:sans-serif nil nil))
                     (clim:present clim:command
                              `(clim:command-name :command-table ,clim:command-table)
                              :stream stream)))
                 (clim:formatting-cell (stream)
                   (clim:with-drawing-options (stream :ink clim:+dark-blue+
                                                 :text-style '(:fix nil nil))
                     (format stream "~&~{~A~^ ~}"
                             (mapcar #'gesture-name (reverse keys))))))
          count clim:command into length
          finally (clim:change-space-requirements stream
                                             :height (* length (clim:stream-line-height stream)))
                  (clim:scroll-extent stream 0 0))))

(defun print-docstring-for-command (clim:command-name clim:command-table &optional (stream *standard-output*))
  "Print documentation for `command-name', which should
   be a symbol bound to a function, to `stream'. If no
   documentation can be found, this fact will be printed to the stream."
  (declare (ignore clim:command-table))
  ;; This needs more regex magic. Also, it is only an interim
  ;; solution.
  (clim:with-text-style (stream '(:sans-serif nil nil))
    (let* ((command-documentation (or (documentation clim:command-name 'function)
                                      "This command is not documented."))
           (first-newline (position #\Newline command-documentation))
           (first-line (subseq command-documentation 0 first-newline)))
      ;; First line is special
      (format stream "~A~%" first-line)
      (when first-newline
        (let* ((rest (subseq command-documentation first-newline))
               (paras (delete ""
                              (loop for start = 0 then (+ 2 end)
                                    for end = (search '(#\Newline #\Newline) rest :start2 start)
                                    collecting
                                    (nsubstitute #\Space #\Newline (subseq rest start end))
                                    while end)
                              :test #'string=)))
          (dolist (para paras)
            (terpri stream)
            (let ((words (loop with length = (length para)
                               with index = 0
                               with start = 0
                               while (< index length)
                               do (loop until (>= index length)
                                        while (member (char para index) '(#\Space #\Tab))
                                        do (incf index))
                                  (setf start index)
                                  (loop until (>= index length)
                                        until (member (char para index) '(#\Space #\Tab))
                                        do (incf index))
                               until (= start index)
                               collecting (string-trim '(#\Space #\Tab #\Newline)
                                                       (subseq para start index)))))
              (loop with margin = (clim:stream-text-margin stream)
                    with space-width = (clim:stream-character-width stream #\Space)
                    with current-width = 0
                    for word in words
                    for word-width = (clim:stream-string-width stream word)
                    when (> (+ word-width current-width)
                            margin)
                      do (terpri stream)
                         (setf current-width 0)
                    do (princ word stream)
                       (princ #\Space stream)
                       (incf current-width (+ word-width space-width))))
            (terpri stream)))))))

(defun describe-command-binding-to-stream (gesture clim:command &key
                                                             (clim:command-table (find-applicable-command-table clim:*application-frame*))
                                                             (stream *standard-output*))
  "Describe `command' as invoked by `gesture' to `stream'."
  (let* ((clim:command-name (if (listp clim:command)
                           (first clim:command)
                           clim:command))
         (command-args (if (listp clim:command)
                           (rest clim:command)))
         (real-command-table (or (clim:command-accessible-in-command-table-p
                                  clim:command-name
                                  clim:command-table)
                                 clim:command-table)))
    (clim:with-text-style (stream '(:sans-serif nil nil))
      (princ "The gesture " stream)
      (clim:with-drawing-options (stream :ink clim:+dark-blue+
                                    :text-style '(:fix nil nil))
        (princ gesture stream))
      (princ " is bound to the command " stream)
      (if (clim:command-present-in-command-table-p clim:command-name real-command-table)
          (clim:with-text-style (stream '(nil :bold nil))
            (clim:present clim:command-name `(clim:command-name :command-table ,clim:command-table) :stream stream))
          (clim:present clim:command-name 'symbol :stream stream))
      (princ " in " stream)
      (clim:present real-command-table 'clim:command-table :stream stream)
      (format stream ".~%")
      (when command-args
        (apply #'format stream
               "This binding invokes the command with these arguments: ~@{~A~^, ~}.~%"
               (mapcar #'(lambda (arg)
                           (cond ((eq arg clim:*unsupplied-argument-marker*)
                                  "unsupplied-argument")
                                 ((eq arg clim:*numeric-argument-marker*)
                                  "numeric-argument")
                                 (t arg))) command-args)))
      (terpri stream)
      (print-docstring-for-command clim:command-name clim:command-table stream)
      (clim:scroll-extent stream 0 0))))

(defun describe-command-to-stream
    (clim:command-name &key
                    (clim:command-table (find-applicable-command-table clim:*application-frame*))
                    (stream *standard-output*))
  "Describe `command' to `stream'."
  (let ((keystrokes (find-keystrokes-for-command-with-inheritance clim:command-name clim:command-table)))
    (clim:with-text-style (stream '(:sans-serif nil nil))
      (clim:with-text-style (stream '(nil :bold nil))
        (clim:present clim:command-name `(clim:command-name :command-table ,clim:command-table) :stream stream))
      (princ " calls the function " stream)
      (clim:present clim:command-name 'symbol :stream stream)
      (princ " and is accessible in " stream)
      (if (clim:command-accessible-in-command-table-p clim:command-name clim:command-table)
          (clim:present (clim:command-accessible-in-command-table-p clim:command-name clim:command-table)
                   'clim:command-table
                   :stream stream)
          (princ "an unknown command table" stream))

      (format stream ".~%")
      (when (plusp (length keystrokes))
        (princ "It is bound to " stream)
        (loop for gestures-list on (first keystrokes)
              do (clim:with-drawing-options (stream :ink clim:+dark-blue+
                                               :text-style '(:fix nil nil))
                   (format stream "~{~A~^ ~}"
                           (mapcar #'gesture-name (reverse (first gestures-list)))))
              when (not (null (rest gestures-list)))
                do (princ ", " stream))
        (terpri stream))
      (terpri stream)
      (print-docstring-for-command clim:command-name clim:command-table stream)
      (clim:scroll-extent stream 0 0))))
