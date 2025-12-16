(cl:in-package #:esclados)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Help

(defgeneric invoke-with-help-stream (esclados title continuation))

(setf (documentation 'invoke-with-help-stream 'function)
      (format nil "Invoke CONTINUATION with a single argument - a stream~@
                  for writing on-line help for ESCLADOS onto. The stream~@
                  should have the title, or name, TITLE (a string), but~@
                  the specific meaning of this is left to the respective~@
                  ESCLADOS."))

(defmethod invoke-with-help-stream (frame title continuation)
  (declare (ignore frame))
  (funcall continuation
           (clim:open-window-stream
            :label title
            :input-buffer (climi::frame-event-queue clim:*application-frame*)
            :width 400)))

(defmacro with-help-stream ((stream title) &body body)
  `(invoke-with-help-stream *esclados-instance* ,title
                            #'(lambda (,stream)
                                ,@body)))

(setf (documentation 'with-help-stream 'function)
      (format nil "Evaluate `body' with `stream' bound to a stream~@
                   suitable for writing help information on. `Title'~@
                   must evaluate to a string, and will be used for~@
                   naming the resulting stream, if that makes sense~@
                   for the ESCLADOS."))

(defun read-gestures-for-help (command-table)
  (clim:with-input-focus (t)
    (loop for gestures = (list (read-gesture))
            then (nconc gestures (list (read-gesture)))
          for item = (find-gestures-with-inheritance gestures command-table)
          unless item
            do (return (values nil gestures))
          when (eq (clim:command-menu-item-type item) :command)
            do (return (values (clim:command-menu-item-value item) gestures)))))

(defun describe-key-briefly (frame)
  (let ((command-table (find-applicable-command-table frame)))
    (multiple-value-bind (command gestures)
        (read-gestures-for-help command-table)
      (when (consp command)
        (setf command (car command)))
      (mini:display-message "~{~A ~}~:[is not bound~;runs the command ~:*~A~]"
                       (mapcar #'gesture-name gestures)
                       (or (clim:command-line-name-for-command
                            command command-table :errorp nil)
                           command)))))

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

(defun find-keystrokes-for-command (command command-table)
  (let ((keystrokes '()))
    (labels ((helper (command command-table prefix)
               (clim:map-over-command-table-keystrokes
                #'(lambda (menu-name keystroke item)
                    (declare (ignore menu-name))
                    (cond ((and (eq (clim:command-menu-item-type item) :command)
                                (or (and (symbolp (clim:command-menu-item-value item))
                                         (eq (clim:command-menu-item-value item) command))
                                    (and (listp (clim:command-menu-item-value item))
                                         (eq (car (clim:command-menu-item-value item)) command))))
                           (push (cons keystroke prefix) keystrokes))
                          ((eq (clim:command-menu-item-type item) :menu)
                           (helper command (clim:command-menu-item-value item) (cons keystroke prefix)))
                          (t nil)))
                command-table)))
      (helper command command-table nil)
      keystrokes)))

(defun find-keystrokes-for-command-with-inheritance (command start-table)
  (let ((keystrokes '()))
    (labels  ((helper (table)
                (let ((keys (find-keystrokes-for-command command table)))
                  (when keys (push keys keystrokes))
                  (dolist (subtable (clim:command-table-inherit-from
                                     (clim:find-command-table table)))
                    (helper subtable)))))
      (helper start-table))
    keystrokes))

(defun find-all-keystrokes-and-commands (command-table)
  (let ((results '()))
    (labels ((helper (command-table prefix)
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
                command-table)))
      (helper command-table nil)
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
     (lambda (command)
       (let ((keys (find-keystrokes-for-command-with-inheritance command start-table)))
         (push (cons command keys) results)))
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

(defun describe-bindings (stream command-table
                          &optional (sort-function #'sort-by-name))
  (clim:formatting-table (stream)
    (loop for (keys . command)
            in (funcall sort-function
                        (find-all-keystrokes-and-commands-with-inheritance
                         command-table))
          when (consp command) do (setq command (car command))
            do (clim:formatting-row (stream)
                 (clim:formatting-cell (stream :align-x :right)
                   (clim:with-text-style (stream '(:sans-serif nil nil))
                     (clim:present command
                              `(clim:command-name :command-table ,command-table)
                              :stream stream)))
                 (clim:formatting-cell (stream)
                   (clim:with-drawing-options (stream :ink clim:+dark-blue+
                                                 :text-style '(:fix nil nil))
                     (format stream "~&~{~A~^ ~}"
                             (mapcar #'gesture-name (reverse keys))))))
          count command into length
          finally (clim:change-space-requirements stream
                                             :height (* length (clim:stream-line-height stream)))
                  (clim:scroll-extent stream 0 0))))

(defun print-docstring-for-command (command-name command-table &optional (stream *standard-output*))
  "Print documentation for `command-name', which should
   be a symbol bound to a function, to `stream'. If no
   documentation can be found, this fact will be printed to the stream."
  (declare (ignore command-table))
  ;; This needs more regex magic. Also, it is only an interim
  ;; solution.
  (clim:with-text-style (stream '(:sans-serif nil nil))
    (let* ((command-documentation (or (documentation command-name 'function)
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

(defun describe-command-binding-to-stream
    (gesture
     command
     &key
       (command-table applicable-command-table)
       (stream *standard-output*))
  "Describe `command' as invoked by `gesture' to `stream'."
  (let* ((command-name (if (listp command)
                           (first command)
                           command))
         (command-args (if (listp command)
                           (rest command)))
         (real-command-table (or (clim:command-accessible-in-command-table-p
                                  command-name
                                  command-table)
                                 command-table)))
    (clim:with-text-style (stream '(:sans-serif nil nil))
      (princ "The gesture " stream)
      (clim:with-drawing-options (stream :ink clim:+dark-blue+
                                    :text-style '(:fix nil nil))
        (princ gesture stream))
      (princ " is bound to the command " stream)
      (if (clim:command-present-in-command-table-p command-name real-command-table)
          (clim:with-text-style (stream '(nil :bold nil))
            (clim:present command-name
                          `(clim:command-name :command-table ,command-table)
                          :stream stream))
          (clim:present command-name 'symbol :stream stream))
      (princ " in " stream)
      (clim:present real-command-table 'command-table :stream stream)
      (format stream ".~%")
      (when command-args
        (apply #'format stream
               "This binding invokes the command with these arguments: ~@{~A~^, ~}.~%"
               (mapcar #'(lambda (arg)
                           (cond ((eq arg clim:*unsupplied-argument-marker*)
                                  "unsupplied-argument")
                                 ((eq arg clim:*numeric-argument-marker*)
                                  "numeric-argument")
                                 (t arg)))
                       command-args)))
      (terpri stream)
      (print-docstring-for-command command-name command-table stream)
      (clim:scroll-extent stream 0 0))))

(defun describe-command-to-stream
    (command-name
     &key
       (command-table applicable-command-table)
       (stream *standard-output*))
  "Describe `command' to `stream'."
  (let ((keystrokes (find-keystrokes-for-command-with-inheritance
                     command-name command-table)))
    (clim:with-text-style (stream '(:sans-serif nil nil))
      (clim:with-text-style (stream '(nil :bold nil))
        (clim:present command-name
                      `(clim:command-name :command-table ,command-table)
                      :stream stream))
      (princ " calls the function " stream)
      (clim:present command-name 'symbol :stream stream)
      (princ " and is accessible in " stream)
      (let ((accessible
              (clim:command-accessible-in-command-table-p
               command-name command-table)))
        (if accessible
          (clim:present accessible 'command-table :stream stream)
          (princ "an unknown command table" stream)))
      (format stream ".~%")
      (when (plusp (length keystrokes))
        (princ "It is bound to " stream)
        (loop for gestures-list on (first keystrokes)
              do (clim:with-drawing-options (stream :ink clim:+dark-blue+
                                               :text-style '(:fix nil nil))
                   (format stream "~{~A~^ ~}"
                           (mapcar #'gesture-name
                                   (reverse (first gestures-list)))))
              when (not (null (rest gestures-list)))
                do (princ ", " stream))
        (terpri stream))
      (terpri stream)
      (print-docstring-for-command command-name command-table stream)
      (clim:scroll-extent stream 0 0))))
