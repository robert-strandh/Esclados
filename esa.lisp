(in-package #:esclados)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ESCLADOS pane mixin

(defclass esclados-pane-mixin ()
  (;; allows a certain number of commands to have some minimal memory
   (%previous-command
    :initform nil
    :accessor previous-command)
   (%command-table
    :initarg :command-table
    :accessor esclados-command-table)))

(defmethod previous-command ((pane clim:pane))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Command processing

(defparameter *esclados-abort-gestures*
  `((:keyboard #\g ,(clim:make-modifier-state :control))))

(defparameter *current-gesture* nil)

(defparameter *command-processor* nil
  "While a command is being run, this symbol will be dynamically
bound to the current command processor.")

(defun find-gestures (gestures start-table)
  (loop with table = (clim:find-command-table start-table)
        for (gesture . rest) on gestures
        for item = (clim:find-keystroke-item  gesture table :errorp nil)
        while item
        do (if (eq (clim:command-menu-item-type item) :command)
               (return (if (null rest) item nil))
               (setf table (clim:command-menu-item-value item)))
        finally (return item)))

(defun find-gestures-with-inheritance (gestures start-table)
  (or (find-gestures gestures start-table)
      (some (lambda (table)
              (find-gestures-with-inheritance gestures table))
            (clim:command-table-inherit-from
             (clim:find-command-table start-table)))))


(defun gesture-matches-gesture-name-p (gesture gesture-name)
  (clim:event-matches-gesture-name-p gesture gesture-name))

(defvar *meta-digit-table*
  (loop for i from 0 to 9
        collect (list :keyboard
                      (digit-char i)
                      (clim:make-modifier-state :meta))))

(defun meta-digit (gesture)
  (position gesture *meta-digit-table*
            :test #'gesture-matches-gesture-name-p))

(defun proper-gesture-p (gesture)
  "Return non-NIL if `gesture' is a proper gesture, NIL
otherwise. A proper gesture is loosely defined as any gesture
that is not just the sole pressing of a modifier key."
  (or (characterp gesture)
      (and (typep gesture 'clim:keyboard-event)
           (or (clim:keyboard-event-character gesture)
               (not (member (clim:keyboard-event-key-name gesture)
                            '(:control-left :control-right
                              :shift-left :shift-right
                              :meta-left :meta-right
                              :super-left :super-right
                              :hyper-left :hyper-right
                              :shift-lock :caps-lock
                              :alt-left :alt-right)))))))

(define-condition unbound-gesture-sequence (simple-condition)
  ((%gestures :initarg :gestures
              :reader gestures
              :initform '()
              :documentation "A list of the provided gestures
that resulted in the signalling of this condition."))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Gesture sequence that cannot
possibly result in command invocation encountered.")))
  (:documentation "This condition is signalled during gesture
processing, when a sequence of gestures has been entered that
does not, and cannot by the addition of more gestures, result in
preferring to a command."))

(defclass command-processor ()
  ((%recordingp :initform nil :accessor recordingp)
   (%executingp :initform nil :accessor executingp)
   (%recorded-keys :initform '() :accessor recorded-keys)
   (%remaining-keys :initform '() :accessor remaining-keys)
   (%accumulated-gestures :initform '() :accessor accumulated-gestures)
   (%overriding-handler :initform nil
                        :accessor overriding-handler
                        :documentation "When non-NIL, any action on
the command processor will be forwarded to this object.")
   (%command-executor :initform 'clim:execute-frame-command
                      :accessor command-executor
                      :initarg :command-executor
                      :documentation "The object used to execute
commands. Will be coerced to a function and called with two
arguments, the command processor and the command."))
  (:documentation "The command processor is fed gestures and will
execute commands or signal conditions when the provided getures
unambigiously suggest one of these actions. ESCLADOS command
processing works through instances of this class."))

(defgeneric process-gesture (command-processor gesture)
  (:documentation "Tell the command processor to process
`gesture'. This might result in either the execution of a command
or the signalling of `unbound-gesture-sequence'. This is the
fundamental interface to the command processor."))

(defgeneric directly-processing-p (command-processor)
  (:documentation "Return true if `command-processor' is directly
  processing commands. In most cases, this means that
  `overriding-handler' is null.")
  (:method ((command-processor command-processor))
    (null (overriding-handler command-processor))))

(defgeneric command-for-unbound-gestures (thing gestures)
  (:documentation "Called when `gestures' is input by the user
and there is no associated command in the current command
table. The function should return either a (possibly incomplete)
command or NIL. In the latter case (which is handled by a default
method), the gestures will be treated as actual unbound
gestures. `Thing' is something that might be interested in
commands, at the beginning usually a command processor, but it
can call the function for other objects it knows in order to get
their opinion. `Gestures' is a list of gestures.")
  (:method (thing gestures)
    (declare (ignore thing gestures))
    nil))

(defclass instant-macro-execution-mixin ()
  ()
  (:documentation "Subclasses of this class will immediately
  process the gestures of a macro when macro processing is
  started by setting `executingp'. This is essential for
  event-based command processing schemes."))

(defmethod (setf executingp) :after ((new-val (eql t)) (drei instant-macro-execution-mixin))
  (loop until (null (remaining-keys drei))
        for gesture = (pop (remaining-keys drei))
        do (process-gesture drei gesture)
        finally (setf (executingp drei) nil)))

(defclass asynchronous-command-processor (command-processor
                                          instant-macro-execution-mixin)
  ()
  (:documentation "Helper class that provides behavior necessary
for a command processor that expects to receive gestures through
asynchronous event handling, and not through
`esclados-read-gesture'."))

(defmethod process-gesture :before
    ((command-processor asynchronous-command-processor) gesture)
  (when (and (find gesture clim:*abort-gestures*
                   :test #'gesture-matches-gesture-name-p)
             (directly-processing-p command-processor))
    (setf (accumulated-gestures command-processor) nil)
    (signal 'clim:abort-gesture :event gesture)))

(defclass dead-key-merging-command-processor (command-processor)
  ((%dead-key-state :accessor dead-key-state
                    :initform nil
                    :documentation "The state of dead key
handling as per `merging-dead-keys'."))
  (:documentation "Helper class useful for asynchronous command
processors, merges incoming dead keys with the following key."))

(defmethod process-gesture :around ((command-processor dead-key-merging-command-processor) gesture)
  (merging-dead-keys (gesture (dead-key-state command-processor))
    (call-next-method command-processor gesture)))

(defclass command-loop-command-processor (command-processor)
  ((%command-table :reader esclados-command-table
                   :initarg :command-table
                   :initform nil)
   (%end-condition :reader end-condition
                   :initarg :end-condition
                   :initform (constantly nil)
                   :documentation "When this function of zero
arguments returns true, the `command-loop-command-processor' will
disable itself in its associated super command processor and call
its `end-function', effectively dropping out of the
sub-command-loop.")
   (%end-function :reader end-function
                  :initarg :end-function
                  :initform (constantly nil)
                  :documentation "This function of zero arguments
will be called when the command processor disables itself.")
   (%abort-function :reader abort-function
                    :initarg :abort-function
                    :initform (constantly nil)
                    :documentation "This function is called if
the command processor encounters an abort gesture.")
   (%super-command-processor :reader super-command-processor
                             :initarg :super-command-processor
                             :initform (error "Must provide a super command processor.")
                             :documentation "The command
processor that the `command-loop-command-processor' object
handles gestures for."))
  (:default-initargs
   :command-executor
   #'(lambda (processor command)
       (funcall
        (command-executor (super-command-processor processor))
        (super-command-processor processor)
        command)))
  (:documentation "This class is used to run sub-command-loops
within the primary command loop of an application (for example,
to do stuff such as incremental search)."))

(defgeneric end-command-loop (command-processor)
  (:documentation "End the simulated command loop controlled by
`command-processor'.")
  (:method ((command-processor command-processor))
    nil))

(defmethod end-command-loop ((command-processor command-loop-command-processor))
  (when (overriding-handler command-processor)
    (end-command-loop (overriding-handler command-processor)))
  (setf (overriding-handler (super-command-processor command-processor)) nil))

(defmethod process-gesture :around
    ((command-processor command-loop-command-processor) gesture)
  (cond ((find gesture clim:*abort-gestures*
               :test #'gesture-matches-gesture-name-p)
         ;; It is to be expected that the abort function might signal
         ;; `abort-gesture'. If that happens, we must end the command
         ;; loop, but ONLY if this is signalled.
         (handler-case (funcall (abort-function command-processor))
           (clim:abort-gesture (c)
             (end-command-loop command-processor)
             (signal c))))
        (t
         (call-next-method)
         (when (funcall (end-condition command-processor))
           (funcall (end-function command-processor))
           (end-command-loop command-processor)))))

(defun process-gestures-for-numeric-argument (gestures)
  "Processes a list of gestures for numeric argument
information. Returns three values: prefix argument, a bool value
indicating whether prefix was given and a list of remaining
gestures to handle. Accepts: EITHER C-u, optionally followed by
other C-u's, optionally followed by a minus sign, optionally
followed by decimal digits; OR An optional M-minus, optionally
followed by M-decimal-digits.  You cannot mix C-u and M-digits.
C-u gives a numarg of 4. Additional C-u's multiply by 4 (e.g. C-u
C-u C-u = 64).  After C-u you can enter decimal digits, possibly
preceded by a minus (but not a plus) sign. C-u 3 4 = 34, C-u - 3
4 = -34. Note that C-u 3 - prints 3 '-'s.  M-1 M-2 = 12. M-- M-1
M-2 = -12. As a special case, C-u - and M-- = -1.  In the absence
of a prefix arg returns 1 (and nil)."
  (let ((first-gesture (pop gestures)))
    (cond ((gesture-matches-gesture-name-p
            first-gesture 'universal-argument)
           (let ((numarg 4))
             (loop for gesture = (first gestures)
                   while (gesture-matches-gesture-name-p
                          gesture 'universal-argument)
                   do (setf numarg (* 4 numarg))
                      (pop gestures))
             (let ((gesture (pop gestures))
                   (sign +1))
               (when (and (characterp gesture)
                          (char= gesture #\-))
                 (setf gesture (pop gestures)
                       sign -1))
               (cond ((and (characterp gesture)
                           (digit-char-p gesture 10))
                      (setf numarg (digit-char-p gesture 10))
                      (loop for gesture = (first gestures)
                            while (and (characterp gesture)
                                       (digit-char-p gesture 10))
                            do (setf numarg (+ (* 10 numarg)
                                               (digit-char-p gesture 10)))
                               (pop gestures)
                            finally (return (values (* numarg sign) t gestures))))
                     (t
                      (values (if (minusp sign) -1 numarg) t
                              (when gesture
                                (cons gesture gestures))))))))
          ((or (meta-digit first-gesture)
               (gesture-matches-gesture-name-p
                first-gesture 'meta-minus))
           (let ((numarg 0)
                 (sign +1))
             (cond ((meta-digit first-gesture)
                    (setf numarg (meta-digit first-gesture)))
                   (t (setf sign -1)))
             (loop for gesture = (first gestures)
                   while (meta-digit gesture)
                   do (setf numarg (+ (* 10 numarg) (meta-digit gesture)))
                      (pop gestures)
                   finally (return (values (if (and (= sign -1) (= numarg 0))
                                               -1
                                               (* sign numarg))
                                           t gestures)))))
          (t (values 1 nil (when first-gesture
                             (cons first-gesture gestures)))))))

(defgeneric process-gestures (command-processor)
  (:documentation "Process the gestures accumulated in
`command-processor', returning T if there are no gestures
accumulated or the accumulated gestures correspond to a
command. In this case, the command will also be executed and the
list of accumulated gestures set to NIL. Will return NIL if the
accumulated gestures do not yet correspond to a command, but
eventually could, if more gestures are provided. Signals
`unbound-gesture-sequence' if the accumulated gestures could
never refer to a command."))

(defmethod process-gestures ((command-processor command-processor))
  (multiple-value-bind (prefix-arg prefix-p gestures)
      (process-gestures-for-numeric-argument
       (accumulated-gestures command-processor))
    (flet ((commandp (object)
             (or (listp object) (symbolp object))))
      (cond ((null gestures)
             t)
            (t
             (let* ((command-table (esclados-command-table command-processor))
                    (item (or (find-gestures-with-inheritance gestures command-table)
                              (command-for-unbound-gestures command-processor gestures))))
               (cond
                 ((not item)
                  (setf (accumulated-gestures command-processor) nil)
                  (error 'unbound-gesture-sequence :gestures gestures))
                 ((or (commandp item) ; c-f-u-g does not return a menu-item.
                      (eq (clim:command-menu-item-type item) :command))
                  (let ((command (if (commandp item) item
                                     (clim:command-menu-item-value item)))
                        (*current-gesture* (first (last gestures)))
                        (*standard-input* (or *minibuffer* *standard-input*)))
                    (unless (consp command)
                      (setf command (list command)))
                    ;; Call `*partial-command-parser*' to handle numeric
                    ;; argument.
                    (unwind-protect
                         (setq command
                               (funcall clim:*partial-command-parser*
                                        (esclados-command-table command-processor)
                                        *standard-input*
                                        command 0 (when prefix-p prefix-arg)))
                      ;; If we are macrorecording, store whatever the user
                      ;; did to invoke this command.
                      (when (recordingp command-processor)
                        (setf (recorded-keys command-processor)
                              (append (accumulated-gestures command-processor)
                                      (recorded-keys command-processor))))
                      (setf (accumulated-gestures command-processor) nil))
                    (funcall (command-executor command-processor) command-processor command)
                    nil))
                 (t t))))))))

(defmethod process-gesture :around ((command-processor command-processor) gesture)
  (with-accessors ((overriding-handler overriding-handler)) command-processor
    (if overriding-handler
        (let ((*command-processor* overriding-handler))
          (process-gesture overriding-handler gesture))
        (call-next-method))))

(defmethod process-gesture ((command-processor command-processor) gesture)
  (setf (accumulated-gestures command-processor)
        (nconc (accumulated-gestures command-processor)
               (list gesture)))
  (process-gestures command-processor))

(defun esclados-read-gesture (&key (command-processor *command-processor*)
                           (stream *standard-input*))
  (unless (null (remaining-keys command-processor))
    (return-from esclados-read-gesture
      (pop (remaining-keys command-processor))))
  (loop for gesture = (clim:read-gesture :stream stream)
        until (proper-gesture-p gesture)
        finally (return gesture)))

(defun esclados-unread-gesture (gesture &key (command-processor *command-processor*)
                                     (stream *standard-input*))
  (cond ((recordingp command-processor)
         (cond ((equal (first (recorded-keys command-processor)) gesture)
                (pop (recorded-keys command-processor)))
               ((equal (first (accumulated-gestures command-processor)) gesture)
                (pop (accumulated-gestures command-processor))))
         (clim:unread-gesture gesture :stream stream))
        ((executingp command-processor)
         (push gesture (remaining-keys command-processor)))
        (t
         (clim:unread-gesture gesture :stream stream))))

(clim:define-gesture-name universal-argument :keyboard (#\u :control))

(clim:define-gesture-name meta-minus :keyboard (#\- :meta))

(defgeneric process-gestures-or-command (command-processor)
  (:documentation "Process gestures for
`command-processor' (typically an application frame), look up the
corresponding commands in `command-table' and invoke them using
`command-executor'."))

(defmethod process-gestures-or-command :around ((command-processor clim:application-frame))
  (clim:with-input-context
      (`(clim:command :command-table ,(esclados-command-table command-processor)))
      (object)
      (call-next-method)
    (clim:command
     (funcall (command-executor command-processor)
              command-processor
              (climi::ensure-complete-command
               object
               (esclados-command-table command-processor)
               *standard-input*)))))

(defmethod process-gestures-or-command :around ((command-processor command-processor))
  (handler-case (call-next-method)
    (clim:abort-gesture (c)
      ;; If the user aborts, we want to forget whatever previous
      ;; gestures he entered since the last command execution.
      (setf (accumulated-gestures command-processor) nil)
      (signal c))))

(defmethod process-gestures-or-command ((command-processor command-processor))
  ;; Build up a list of gestures and repeatedly pass them to
  ;; `process-gestures'. This "clumsy" approach is chosen because we
  ;; want ESCLADOS command processing to support asynchronous operation as
  ;; well, something that either requires this kind of repeated
  ;; rescanning of accumulated input data or some yet-unimplemented
  ;; complex state retaining mechanism (such as continuations).
  (loop (let ((*current-gesture* (esclados-read-gesture :command-processor command-processor)))
          (unless (process-gesture command-processor *current-gesture*)
            (return)))))

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
     ,(let ((frame (unlisted frame)))
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

;;; help commands

(clim:define-command-table help-table)

(clim:define-command (com-describe-key-briefly :name t :command-table help-table) ()
  "Prompt for a key and show the command it invokes."
  (display-message "Describe key briefly:")
  (clim:redisplay-frame-panes clim:*application-frame*)
  (describe-key-briefly clim:*application-frame*))

(set-key 'com-describe-key-briefly 'help-table '((#\h :control) (#\c)))

(clim:define-command (com-where-is :name t :command-table help-table) ()
  "Prompt for a command name and show the key that invokes it."
  (let* ((clim:command-table (find-applicable-command-table clim:*application-frame*))
         (clim:command
           (handler-case
               (clim:accept
                `(clim:command-name :command-table
                               ,clim:command-table)
                :prompt "Where is command")
             (error () (progn (clim:beep)
                              (display-message "No such command")
                              (return-from com-where-is nil)))))
         (keystrokes (find-keystrokes-for-command-with-inheritance clim:command clim:command-table)))
    (display-message "~A is ~:[not on any key~;~:*on ~{~A~^, ~}~]"
                     (clim:command-line-name-for-command clim:command clim:command-table)
                     (mapcar (lambda (keys)
                               (format nil "~{~A~^ ~}"
                                       (mapcar #'gesture-name (reverse keys))))
                             (car keystrokes)))))

(set-key 'com-where-is 'help-table '((#\h :control) (#\w)))

(clim:define-command (com-describe-bindings :name t :command-table help-table)
    ((sort-by-keystrokes 'boolean :prompt "Sort by keystrokes?"))
  "Show which keys invoke which commands.
Without a numeric prefix, sorts the list by command name. With a numeric prefix, sorts by key."
  (let ((clim:command-table (find-applicable-command-table clim:*application-frame*)))
    (with-help-stream (stream (format nil "Help: Describe Bindings"))
      (describe-bindings stream clim:command-table
                         (if sort-by-keystrokes
                             #'sort-by-keystrokes
                             #'sort-by-name)))))

(set-key `(com-describe-bindings ,clim:*numeric-argument-marker*) 'help-table '((#\h :control) (#\b)))

(clim:define-command (com-describe-key :name t :command-table help-table)
    ()
  "Display documentation for the command invoked by a given gesture sequence.
When invoked, this command will wait for user input. If the user inputs a gesture
sequence bound to a command available in the syntax of the current buffer,
documentation and other details will be displayed in a typeout pane."
  (let ((clim:command-table (find-applicable-command-table clim:*application-frame*)))
    (display-message "Describe Key:")
    (clim:redisplay-frame-panes clim:*application-frame*)
    (multiple-value-bind (clim:command gestures)
        (read-gestures-for-help clim:command-table)
      (let ((gesture-name (format nil "~{~A~#[~; ~; ~]~}"
                                  (mapcar #'gesture-name gestures))))
        (if clim:command
            (with-help-stream (out-stream (format nil "~10THelp: Describe Key for ~A" gesture-name))
              (describe-command-binding-to-stream gesture-name clim:command
                                                  :command-table clim:command-table
                                                  :stream out-stream))
            (display-message "Unbound gesture: ~A" gesture-name))))))

(set-key 'com-describe-key
         'help-table
         '((#\h :control) (#\k)))

(clim:define-command (com-describe-command :name t :command-table help-table)
    ((clim:command 'clim:command-name :prompt "Describe command"))
  "Display documentation for the given command."
  (let ((clim:command-table (find-applicable-command-table clim:*application-frame*)))
    (with-help-stream (out-stream (format nil "~10THelp: Describe Command for ~A"
                                          (clim:command-line-name-for-command clim:command
                                                                         clim:command-table
                                                                         :errorp nil)))
      (describe-command-to-stream clim:command
                                  :command-table clim:command-table
                                  :stream out-stream))))

(set-key `(com-describe-command ,clim:*unsupplied-argument-marker*)
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

(clim:define-command (com-apropos-command :name t :command-table help-table)
    ((words '(sequence string) :prompt "Search word(s)"))
  "Shows commands with documentation matching the search words.
Words are comma delimited. When more than two words are given, the documentation must match any two."
  ;; 23.8.6 "It is unspecified whether accept returns a list or a vector."
  (setf words (coerce words 'list))
  (when words
    (let* ((command-table (find-applicable-command-table clim:*application-frame*))
           (results (find-command-key-pairs words command-table)))
      (if (null results)
          (display-message "No results for ~{~A~^, ~}" words)
          (with-help-stream (out-stream (format nil "~10THelp: Apropos ~{~A~^, ~}" words))
            (loop for (command . keys) in results
                  for documentation = (or (documentation command 'function)
                                          "Not documented.")
                  do (clim:with-text-style (out-stream '(:sans-serif :bold nil))
                       (clim:present command
                                `(clim:command-name :command-table ,command-table)
                                :stream out-stream))
                     (clim:with-drawing-options (out-stream :ink clim:+dark-blue+
                                                       :text-style '(:fix nil nil))
                       (format out-stream "~30T~:[M-x ... RETURN~;~:*~{~A~^, ~}~]"
                               (mapcar (lambda (keystrokes)
                                         (format nil "~{~A~^ ~}"
                                                 (mapcar #'gesture-name (reverse keystrokes))))
                                       (car keys))))
                     (clim:with-text-style (out-stream '(:sans-serif nil nil))
                       (format out-stream "~&~2T~A~%"
                               (subseq documentation 0 (position #\Newline documentation))))
                  count command into length
                  finally (clim:change-space-requirements out-stream
                                                     :height (* length (clim:stream-line-height out-stream)))
                          (clim:scroll-extent out-stream 0 0)))))))

(set-key `(com-apropos-command ,clim:*unsupplied-argument-marker*)
         'help-table
         '((#\h :control) (#\a)))

(define-menu-table help-menu-table (help-table)
  'com-where-is
  '(com-describe-bindings nil)
  '(com-describe-bindings t)
  'com-describe-key
  `(com-describe-command ,clim:*unsupplied-argument-marker*)
  `(com-apropos-command ,clim:*unsupplied-argument-marker*))
