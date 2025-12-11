(cl:in-package #:esclados)

(defparameter *abort-gestures*
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

(setf (documentation 'proper-gesture-p 'function)
      (format nil "Return non-NIL if `gesture' is a proper gesture,~@
                   NIL otherwise. A proper gesture is loosely defined~@
                   as any gesture that is not just the sole pressing~@
                   of a modifier key."))

(define-condition unbound-gesture-sequence (simple-condition)
  ((%gestures :initarg :gestures
              :reader gestures
              :initform '()
              :documentation "A list of the provided gestures
that resulted in the signaling of this condition."))
  (:report "Gesture sequence that cannot possibly result in command invocation encountered."))

(setf (documentation 'unbound-gesture-sequence 'type)
      (format nil "This condition is signalled during gesture processing,~@
                   when a sequence of gestures has been entered that does~@
                   not, and cannot by the addition of more gestures,~@
                   result in preferring to a command."))

(defclass command-processor ()
  ((%recordingp :initform nil :accessor recordingp)
   (%executingp :initform nil :accessor executingp)
   (%recorded-keys :initform '() :accessor recorded-keys)
   (%remaining-keys :initform '() :accessor remaining-keys)
   (%accumulated-gestures :initform '() :accessor accumulated-gestures)
   ;; When this slot contains an object other than NIL, any action on
   ;; the command processor will be forwarded to this object.  In
   ;; other words, whatever operator uses an instance of this class as
   ;; an argument, a recursive call to that operator will be made,
   ;; where the instance of this class is replaced by the object in
   ;; this slot.
   (%overriding-handler :initform nil
                        :accessor overriding-handler)
   ;; This slot contains the object used to execute commands.  It is
   ;; coerced to be a function, and that function is called with two
   ;; arguments, the command processor and the command.
   (%command-executor :initform 'clim:execute-frame-command
                      :accessor command-executor
                      :initarg :command-executor)))

(setf (documentation 'command-processor 'type)
      (format nil "The command processor is fed gestures and will~@
                   execute commands or signal conditions when the~@
                   provided getures unambigiously suggest one of~@
                   these actions. ESCLADOS command processing works~@
                   through instances of this class."))

(defgeneric process-gesture (command-processor gesture))

(setf (documentation 'process-gesture 'function)
      (format nil "Tell the command processor to process `gesture'.~@
                   This might result in either the execution of a command~@
                   or the signaling of `unbound-gesture-sequence'.~@
                   This is the fundamental interface to the command~@
                   processor."))

(defgeneric directly-processing-p (command-processor)
  (:method ((command-processor command-processor))
    (null (overriding-handler command-processor))))

(setf (documentation 'directly-processing-p 'function)
      (format nil "Return true if `command-processor' is directly~@
                   processing commands. In most cases, this means that~@
                   `overriding-handler' is null."))

(defgeneric command-for-unbound-gestures (thing gestures)
  (:method (thing gestures)
    (declare (ignore thing gestures))
    nil))

(setf (documentation 'command-for-unbound-gestures 'function)
      (format nil "Called when `gestures' is input by the user and~@
                   there is no associated command in the current~@
                   command table. The function should return either~@
                   a (possibly incomplete) command or NIL. In the latter~@
                   case (which is handled by a default method), the~@
                   gestures will be treated as actual unbound gestures.~@
                   `Thing' is something that might be interested in~@
                   commands, at the beginning usually a command processor,~@
                   but it can call the function for other objects it knows~@
                   in order to get their opinion. `Gestures' is a list~@
                   of gestures."))

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
  ((%command-table
    :reader esclados-command-table
    :initarg :command-table
    :initform nil)
   (%end-condition
    :reader end-condition
    :initarg :end-condition
    :initform (constantly nil)
    :documentation "When this function of zero
arguments returns true, the `command-loop-command-processor' will
disable itself in its associated super command processor and call
its `end-function', effectively dropping out of the
sub-command-loop.")
   (%end-function
    :reader end-function
    :initarg :end-function
    :initform (constantly nil)
    :documentation "This function of zero arguments
will be called when the command processor disables itself.")
   (%abort-function
    :reader abort-function
    :initarg :abort-function
    :initform (constantly nil)
    :documentation "This function is called if
the command processor encounters an abort gesture.")
   (%super-command-processor
    :reader super-command-processor
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

(defmethod end-command-loop
    ((command-processor command-loop-command-processor))
  (when (overriding-handler command-processor)
    (end-command-loop (overriding-handler command-processor)))
  (setf (overriding-handler (super-command-processor command-processor))
        nil))

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

(setf (documentation 'process-gestures-for-numeric-argument 'function)
      (format nil "Processes a list of gestures for numeric argument~@
                   information. Returns three values: prefix argument,~@
                   a Boolean value indicating whether prefix was given~@
                   and a list of remaining gestures to handle. Accepts:~@
                   EITHER C-u, optionally followed by other C-u's,~@
                   optionally followed by a minus sign, optionally followed~@
                   by decimal digits; OR An optional M-minus, optionally~@
                   followed by M-decimal-digits.  You cannot mix C-u and~@
                   M-digits. C-u gives a numarg of 4. Additional C-u's~@
                   multiply by 4 (e.g. C-u C-u C-u = 64).  After C-u you~@
                   can enter decimal digits, possibly preceded by a minus~@
                   (but not a plus) sign. C-u 3 4 = 34, C-u - 3 4 = -34.~@
                   Note that C-u 3 - prints 3 '-'s.  M-1 M-2 = 12. M-- M-1~@
                   M-2 = -12. As a special case, C-u - and M-- = -1.  In~@
                   the absence of a prefix arg returns 1 (and nil)."))

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

