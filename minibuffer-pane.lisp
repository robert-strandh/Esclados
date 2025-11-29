(cl:in-package #:esclados)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Minibuffer pane

(defgeneric minibuffer (application-frame)
  (:documentation "Return the minibuffer of APPLICATION-FRAME."))

(defvar *minibuffer* nil
  "The minibuffer pane of the running application.")

(defvar *minimum-message-time* 1
  "The minimum number of seconds a minibuffer message will be
  displayed." )

(defclass minibuffer-pane (application-pane)
  ((message :initform nil
            :accessor message
            :documentation "An output record containing whatever
message is supposed to be displayed in the minibuffer.")
   (message-time :initform 0
                 :accessor message-time
                 :documentation "The universal time at which the
current message was set."))
  (:default-initargs
   :display-function 'display-minibuffer
   :display-time :command-loop
   :incremental-redisplay t))

(defmethod handle-repaint ((pane minibuffer-pane) region)
  (declare (ignore region))
  (when (and (message pane)
             (> (get-universal-time)
                (+ *minimum-message-time* (message-time pane))))
    ;; We are no longer allowed to call WINDOW-CLEAR from the
    ;; application thread.
    ;; (window-clear pane)
    (setf (message pane) nil))
  (call-next-method))

(defmethod (setf message) :after (new-value (pane minibuffer-pane))
  (declare (ignore new-value))
  (change-space-requirements pane))

(defmethod pane-needs-redisplay ((pane minibuffer-pane))
  ;; Always call the display function, never clear the window. This
  ;; allows us to time-out the message in the minibuffer.
  (values t nil))

(defun display-minibuffer (frame pane)
  (declare (ignore frame))
  ;; We are probably no longer allowed to call dispatch-repaint in the
  ;; main thread of the application.
  ;; (dispatch-repaint pane +everywhere+))
  (finish-output pane))

(defmethod stream-accept :around ((pane minibuffer-pane) type &rest args)
  (declare (ignore type args))
  (when (message pane)
    (setf (message pane) nil))
  (window-clear pane)
  ;; FIXME: this isn't the friendliest way of indicating a parse
  ;; error: there's no feedback, unlike emacs' quite nice "[no
  ;; match]".
  (unwind-protect
       (loop
         (handler-case
             (with-input-focus (pane)
               (return (call-next-method)))
           (parse-error () nil)))
    (window-clear pane)))

(defmethod stream-accept ((pane minibuffer-pane) type &rest args
                          &key (view (stream-default-view pane))
                          &allow-other-keys)
  ;; default CLIM prompting is OK for now...
  (apply #'prompt-for-accept pane type view args)
  ;; but we need to turn some of ACCEPT-1 off.
  (apply #'accept-1-for-minibuffer pane type args))

(defmethod compose-space ((pane minibuffer-pane) &key width height)
  (declare (ignore width height))
  (with-sheet-medium (medium pane)
    (let* ((sr (call-next-method))
           ;; We are no longer allowed to call text-style-height at
           ;; this point, because the medium is a BASIC-MEDIUM when we
           ;; start the application. 
           ;; (height (max (text-style-height (medium-merged-text-style medium)
           ;;                                 medium)
           ;;              (bounding-rectangle-height (stream-output-history pane)))))
           (height 100))
      (make-space-requirement
       :height height :min-height height :max-height height
       :width (space-requirement-width sr)
       :min-width (space-requirement-min-width sr)
       :max-width (space-requirement-max-width sr)))))

;;; simpler version of McCLIM's internal operators of the same names:
;;; HANDLE-EMPTY-INPUT to make default processing work, EMPTY-INPUT-P
;;; and INVOKE-HANDLE-EMPTY-INPUT to support it.  We don't support
;;; recursive bouncing to see who most wants to handle the empty
;;; input, but that's OK, because we are always conceptually one-level
;;; deep in accept (even if sometimes we call ACCEPT recursively for
;;; e.g. command-names and arguments).
(defmacro handle-empty-input ((stream) input-form &body handler-forms)
  "see climi::handle-empty-input"
  (let ((input-cont (gensym "INPUT-CONT"))
        (handler-cont (gensym "HANDLER-CONT")))
    `(flet ((,input-cont ()
              ,input-form)
            (,handler-cont ()
              ,@handler-forms))
       (declare (dynamic-extent #',input-cont #',handler-cont))
       (invoke-handle-empty-input ,stream #',input-cont #',handler-cont))))

;;; The code that signalled the error might have consumed the gesture, or
;;; not.
;;; XXX Actually, it would be a violation of the `accept' protocol to consume
;;; the gesture, but who knows what random accept methods are doing.
(defun empty-input-p
    (stream begin-scan-pointer activation-gestures delimiter-gestures)
  (let ((scan-pointer (stream-scan-pointer stream))
        (fill-pointer (fill-pointer (stream-input-buffer stream))))
    ;; activated?
    (cond ((and (eql begin-scan-pointer scan-pointer)
                (eql scan-pointer fill-pointer))
           t)
          ((or (eql begin-scan-pointer scan-pointer)
               (eql begin-scan-pointer (1- scan-pointer)))
           (let ((gesture
                   (aref (stream-input-buffer stream) begin-scan-pointer)))
             (and (characterp gesture)
                  (flet ((gesture-matches-p (g)
                           (if (characterp g)
                               (char= gesture g)
                               ;; FIXME: not quite portable --
                               ;; apparently
                               ;; EVENT-MATCHES-GESTURE-NAME-P need
                               ;; not work on raw characters
                               (event-matches-gesture-name-p gesture g))))
                    (or (some #'gesture-matches-p activation-gestures)
                        (some #'gesture-matches-p delimiter-gestures))))))
          (t nil))))

(defun invoke-handle-empty-input
    (stream input-continuation handler-continuation)
  (unless (input-editing-stream-p stream)
    (return-from invoke-handle-empty-input (funcall input-continuation)))
  (let ((begin-scan-pointer (stream-scan-pointer stream))
        (activation-gestures *activation-gestures*)
        (delimiter-gestures *delimiter-gestures*))
    (block empty-input
      (handler-bind
          ((parse-error
             #'(lambda (c)
                 (declare (ignore c))
                 (when (empty-input-p stream begin-scan-pointer
                                      activation-gestures delimiter-gestures)
                   (return-from empty-input nil)))))
        (return-from invoke-handle-empty-input (funcall input-continuation))))
    (funcall handler-continuation)))

(defun accept-1-for-minibuffer
    (stream type
     &key
       (view (stream-default-view stream))
       (default nil defaultp) (default-type nil default-type-p)
       provide-default insert-default (replace-input t)
       history active-p prompt prompt-mode display-default
       query-identifier (activation-gestures nil activationsp)
       (additional-activation-gestures nil additional-activations-p)
       (delimiter-gestures nil delimitersp)
       (additional-delimiter-gestures nil  additional-delimiters-p))
  (declare (ignore provide-default history active-p
                   prompt prompt-mode
                   display-default query-identifier))
  (when (and defaultp (not default-type-p))
    (error ":default specified without :default-type"))
  (when (and activationsp additional-activations-p)
    (error "only one of :activation-gestures or ~
            :additional-activation-gestures may be passed to accept."))
  (unless (or activationsp additional-activations-p *activation-gestures*)
    (setq activation-gestures *standard-activation-gestures*))
  (with-input-editing
      ;; this is the main change from CLIM:ACCEPT-1 -- no sensitizer.
      (stream :input-sensitizer nil)
    ;; KLUDGE: no call to CLIMI::WITH-INPUT-POSITION here, but that's
    ;; OK because we are always going to create a new editing stream
    ;; for each call to accept/accept-1-for-minibuffer, so the default
    ;; default for the BUFFER-START argument to REPLACE-INPUT is
    ;; right.
    (when (and insert-default
               (not (stream-rescanning-p stream)))
      ;; Insert the default value to the input stream. It should
      ;; become fully keyboard-editable. We do not want to insert
      ;; the default if we're rescanning, only during initial
      ;; setup.
      (presentation-replace-input stream default default-type view))
    (with-input-context (type)
        (object object-type event options)
        (with-activation-gestures ((if additional-activations-p
                                       additional-activation-gestures
                                       activation-gestures)
                                   :override activationsp)
          (with-delimiter-gestures ((if additional-delimiters-p
                                        additional-delimiter-gestures
                                        delimiter-gestures)
                                    :override delimitersp)
            (let ((accept-results nil))
              (climi::handle-empty-input
                  (stream)
                  (setq accept-results
                        (multiple-value-list
                         (if defaultp
                             (funcall-presentation-generic-function
                              accept type stream view
                              :default default :default-type default-type)
                             (funcall-presentation-generic-function
                              accept type stream view))))
                ;; User entered activation or delimiter gesture
                ;; without any input.
                (if defaultp
                    (presentation-replace-input
                     stream default default-type view :rescan nil)
                    (simple-parse-error
                     "Empty input for type ~S with no supplied default"
                     type))
                (setq accept-results (list default default-type)))
              ;; Eat trailing activation gesture
              ;; XXX what about pointer gestures?
              ;; XXX and delimiter gestures?
              ;;
              ;; deleted check for *RECURSIVE-ACCEPT-P*
              (let ((ag (read-gesture :stream stream :timeout 0)))
                (unless (or (null ag) (eq ag stream))
                  (unless (activation-gesture-p ag)
                    (unread-gesture ag :stream stream))))
              (values (car accept-results)
                      (if (cdr accept-results) (cadr accept-results) type)))))
      ;; A presentation was clicked on, or something.
      (t
       (when (and replace-input
                  (getf options :echo t)
                  (not (stream-rescanning-p stream)))
         (presentation-replace-input
          stream object object-type view :rescan nil))
       (values object object-type)))))

(defgeneric invoke-with-minibuffer-stream (minibuffer continuation))

(defmethod invoke-with-minibuffer-stream ((minibuffer minibuffer-pane) continuation)
  (window-clear minibuffer)
  (setf (message minibuffer)
        (with-new-output-record (minibuffer)
          (setf (message-time minibuffer) (get-universal-time))
          (filling-output (minibuffer :fill-width (bounding-rectangle-width minibuffer))
            (funcall continuation minibuffer)))))

(defmethod invoke-with-minibuffer-stream ((minibuffer pointer-documentation-pane) continuation)
  (funcall continuation minibuffer))

(defmethod invoke-with-minibuffer-stream ((minibuffer null) continuation)
  (declare (ignore continuation))
  nil)

(defmacro with-minibuffer-stream ((stream-symbol)
                                  &body body)
  "Bind `stream-symbol' to the minibuffer stream and evaluate
  `body'. This macro makes sure to setup the initial blanking of
  the minibuffer as well as taking care of for how long the
  message should be displayed."
  `(invoke-with-minibuffer-stream *minibuffer*
                                  #'(lambda (,stream-symbol)
                                      ,@body)))

(defun display-message (format-string &rest format-args)
  "Display a message in the minibuffer. Composes the string based
on the `format-string' and the `format-args'."
  (with-minibuffer-stream (minibuffer)
    (apply #'format minibuffer format-string format-args)))
