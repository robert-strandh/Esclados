(cl:in-package #:esclados-help)

(clim:define-command-table help-table)

;;; describe-key-briefly

(clim:define-command
    (com-describe-key-briefly :name t :command-table help-table)
    ()
  (mini:display-message "Describe key briefly:")
  (clim:redisplay-frame-panes clim:*application-frame*)
  (describe-key-briefly))

(setf (documentation 'com-describe-key-briefly 'function)
      (format nil "Prompt for a key and show the command it invokes."))

(tbl:set-key 'com-describe-key-briefly 'help-table '((#\h :control) (#\c)))

;;; where-is

(clim:define-command (com-where-is :name t :command-table help-table) ()
  (let* ((command-table frame:applicable-command-table)
         (command
           (handler-case
               (clim:accept
                `(clim:command-name :command-table ,command-table)
                :prompt "Where is command")
             (error () (progn (clim:beep)
                              (mini:display-message "No such command")
                              (return-from com-where-is nil)))))
         (keystrokes (find-keystrokes-for-command-with-inheritance
                      command command-table)))
    (mini:display-message
     "~A is ~:[not on any key~;~:*on ~{~A~^, ~}~]"
     (clim:command-line-name-for-command command command-table)
     (mapcar (lambda (keys)
               (format nil "~{~A~^ ~}"
                       (mapcar #'utils:gesture-name (reverse keys))))
             (car keystrokes)))))

(setf (documentation 'com-where-is 'function)
      (format nil "Prompt for a command name and show the key~@
                   that invokes it."))

(tbl:set-key 'com-where-is 'help-table '((#\h :control) (#\w)))

;;; describe-bindings

(clim:define-command
    (com-describe-bindings :name t :command-table help-table)
    ((sort-by-keystrokes 'boolean :prompt "Sort by keystrokes?"))
  (let ((command-table frame:applicable-command-table))
    (with-help-stream (stream (format nil "Help: Describe Bindings"))
      (describe-bindings stream command-table
                         (if sort-by-keystrokes
                             #'sort-by-keystrokes
                             #'sort-by-name)))))

(setf (documentation 'com-describe-bindings 'function)
      (format nil "Show which keys invoke which commands.~@
                   Without a numeric prefix, sorts the list by~@
                   command name. With a numeric prefix, sorts by key."))

(tbl:set-key `(com-describe-bindings ,clim:*numeric-argument-marker*)
             'help-table
             '((#\h :control) (#\b)))

;;; describe-key

(clim:define-command (com-describe-key :name t :command-table help-table)
    ()
  (let ((command-table frame:applicable-command-table))
    (mini:display-message "Describe Key:")
    (clim:redisplay-frame-panes clim:*application-frame*)
    (multiple-value-bind (command gestures)
        (read-gestures-for-help command-table)
      (let* ((gesture-name (format nil "~{~A~#[~; ~; ~]~}"
                                   (mapcar #'utils:gesture-name gestures)))
             (prompt (format nil "~10THelp: Describe Key for ~A" gesture-name)))
        (if command
            (with-help-stream (out-stream prompt)
              (describe-command-binding-to-stream
               gesture-name command
               :command-table command-table
               :stream out-stream))
            (mini:display-message "Unbound gesture: ~A" gesture-name))))))

(setf (documentation 'com-describe-key 'function)
      (format nil "Display documentation for the command invoked by~@
                   a given gesture sequence.~@
                   ~@
                   When invoked, this command will wait for user input.~@
                   If the user inputs a gesture sequence bound to a~@
                   command available in the syntax of the current buffer,~@
                   Documentation and other details will be displayed in~@
                   a typeout pane."))

(tbl:set-key 'com-describe-key
             'help-table
             '((#\h :control) (#\k)))

(clim:define-command (com-describe-command :name t :command-table help-table)
    ((command 'clim:command-name :prompt "Describe command"))
  "Display documentation for the given command."
  (let* ((command-table frame:applicable-command-table)
         (prompt "~10THelp: Describe Command for ~A")
         (name (clim:command-line-name-for-command
                command command-table :errorp nil)))
    (with-help-stream (out-stream (format nil prompt name))
      (describe-command-to-stream
       command :command-table command-table :stream out-stream))))

(tbl:set-key `(com-describe-command ,clim:*unsupplied-argument-marker*)
             'help-table
             '((#\h :control) (#\f)))

(clim:define-presentation-to-command-translator describe-command
    (clim:command-name com-describe-command help-table
                  :gesture :select
                  :documentation "Describe command")
    (object)
  (list object))

(defun search-single-word (word function documentation)
  (or (search word (symbol-name function)
              :test #'char-equal)
      (search word documentation :test #'char-equal)))

(defun search-multiple-words (words function documentation)
  (loop with score = 0
        for word in words
        until (> score 1)
        when (or
              (search word (symbol-name function)
                      :test #'char-equal)
              (search word documentation :test #'char-equal))
          do (incf score)
        finally (return (> score 1))))

(defun find-command-key-pairs (words command-table)
  (loop for (function . keys)
          in (find-all-commands-and-keystrokes-with-inheritance
              command-table)
        when (consp function)
          do (setq function (car function))
        when (let ((documentation (or (documentation function 'function) "")))
               (cond
                 ((> (length words) 1)
                  (search-multiple-words words function documentation))
                 (t (search-single-word (first words) function documentation))))
          collect (cons function keys)))

(defun present-command-key-pairs (command-key-pairs command-table stream)
  (loop for (command . keys) in command-key-pairs
        for documentation = (or (documentation command 'function)
                                "Not documented.")
        do (clim:with-text-style (stream '(:sans-serif :bold nil))
             (clim:present command
                           `(clim:command-name :command-table ,command-table)
                           :stream stream))
           (clim:with-drawing-options (stream :ink clim:+dark-blue+
                                              :text-style '(:fix nil nil))
             (format stream "~30T~:[M-x ... RETURN~;~:*~{~A~^, ~}~]"
                     (mapcar (lambda (keystrokes)
                               (format nil "~{~A~^ ~}"
                                       (mapcar #'utils:gesture-name (reverse keystrokes))))
                             (car keys))))
           (clim:with-text-style (stream '(:sans-serif nil nil))
             (format stream "~&~2T~A~%"
                     (subseq documentation 0 (position #\Newline documentation))))
        count command into length
        finally (clim:change-space-requirements
                 stream
                 :height (* length (clim:stream-line-height stream)))
                (clim:scroll-extent stream 0 0)))

(clim:define-command (com-apropos-command :name t :command-table help-table)
    ((words '(sequence string) :prompt "Search word(s)"))
  "Shows commands with documentation matching the search words.
Words are comma delimited. When more than two words are given, the documentation must match any two."
  ;; 23.8.6 "It is unspecified whether accept returns a list or a vector."
  (setf words (coerce words 'list))
  (when words
    (let* ((command-table frame:applicable-command-table)
           (results (find-command-key-pairs words command-table)))
      (if (null results)
          (mini:display-message "No results for ~{~A~^, ~}" words)
          (with-help-stream (out-stream (format nil "~10THelp: Apropos ~{~A~^, ~}" words))
            (present-command-key-pairs results command-table out-stream))))))

(tbl:set-key `(com-apropos-command ,clim:*unsupplied-argument-marker*)
             'help-table
             '((#\h :control) (#\a)))

(utils:define-menu-table help-menu-table (help-table)
  'com-where-is
  '(com-describe-bindings nil)
  '(com-describe-bindings t)
  'com-describe-key
  `(com-describe-command ,clim:*unsupplied-argument-marker*)
  `(com-apropos-command ,clim:*unsupplied-argument-marker*))
