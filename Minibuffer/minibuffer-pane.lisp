(cl:in-package #:esclados-minibuffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Minibuffer pane

(defgeneric minibuffer (clim:application-frame))

(setf (documentation 'minibuffer 'function)
      (format nil "Return the minibuffer of APPLICATION-FRAME."))

(defvar *minibuffer* nil
  "The minibuffer pane of the running application.")

(defvar *minimum-message-time* 1
  "The minimum number of seconds a minibuffer message will be
  displayed." )

(defclass minibuffer-pane (clim:application-pane)
  ((%message :initform nil
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

(defmethod clim:handle-repaint ((clim:pane minibuffer-pane) clim:region)
  (declare (ignore clim:region))
  (when (and (message clim:pane)
             (> (get-universal-time)
                (+ *minimum-message-time* (message-time clim:pane))))
    ;; We are no longer allowed to call WINDOW-CLEAR from the
    ;; application thread.
    ;; (window-clear pane)
    (setf (message clim:pane) nil))
  (call-next-method))

(defmethod (setf message) :after (new-value (clim:pane minibuffer-pane))
  (declare (ignore new-value))
  (clim:change-space-requirements clim:pane))

(defmethod clim:pane-needs-redisplay ((clim:pane minibuffer-pane))
  ;; Always call the display function, never clear the window. This
  ;; allows us to time-out the message in the minibuffer.
  (values t nil))

(defun display-minibuffer (frame clim:pane)
  (declare (ignore frame))
  ;; We are probably no longer allowed to call dispatch-repaint in the
  ;; main thread of the application.
  ;; (dispatch-repaint pane +everywhere+))
  (finish-output clim:pane))

(defmethod clim:stream-accept :around
    ((clim:pane minibuffer-pane) type &rest args)
  (declare (ignore type args))
  (when (message clim:pane)
    (setf (message clim:pane) nil))
  (clim:window-clear clim:pane)
  ;; FIXME: this isn't the friendliest way of indicating a parse
  ;; error: there's no feedback, unlike emacs' quite nice "[no
  ;; match]".
  (unwind-protect
       (loop
         (handler-case
             (clim:with-input-focus (clim:pane)
               (return (call-next-method)))
           (parse-error () nil)))
    (clim:window-clear clim:pane)))

(defmethod clim:stream-accept
    ((clim:pane minibuffer-pane)
     type
     &rest args
     &key (clim:view (clim:stream-default-view clim:pane))
     &allow-other-keys)
  ;; default CLIM prompting is OK for now...
  (apply #'clim:prompt-for-accept clim:pane type clim:view args)
  ;; but we need to turn some of ACCEPT-1 off.
  (apply #'accept-1-for-minibuffer clim:pane type args))

(defmethod clim:compose-space ((clim:pane minibuffer-pane) &key width height)
  (declare (ignore width height))
  (clim:with-sheet-medium (clim:medium clim:pane)
    (let* ((sr (call-next-method))
           ;; We are no longer allowed to call text-style-height at
           ;; this point, because the medium is a BASIC-MEDIUM when we
           ;; start the application. 
           ;; (height (max (text-style-height (medium-merged-text-style medium)
           ;;                                 medium)
           ;;              (bounding-rectangle-height (stream-output-history pane)))))
           (height 100))
      (clim:make-space-requirement
       :height height :min-height height :max-height height
       :width (clim:space-requirement-width sr)
       :min-width (clim:space-requirement-min-width sr)
       :max-width (clim:space-requirement-max-width sr)))))

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

;;; The code that signaled the error might have consumed the gesture, or
;;; not.
;;; XXX Actually, it would be a violation of the `accept' protocol to consume
;;; the gesture, but who knows what random accept methods are doing.
(defun empty-input-p
    (stream begin-scan-pointer activation-gestures delimiter-gestures)
  (let ((scan-pointer (clim:stream-scan-pointer stream))
        (fill-pointer (fill-pointer (clim:stream-input-buffer stream))))
    ;; activated?
    (cond ((and (eql begin-scan-pointer scan-pointer)
                (eql scan-pointer fill-pointer))
           t)
          ((or (eql begin-scan-pointer scan-pointer)
               (eql begin-scan-pointer (1- scan-pointer)))
           (let ((gesture
                   (aref (clim:stream-input-buffer stream) begin-scan-pointer)))
             (and (characterp gesture)
                  (flet ((gesture-matches-p (g)
                           (if (characterp g)
                               (char= gesture g)
                               ;; FIXME: not quite portable --
                               ;; apparently
                               ;; EVENT-MATCHES-GESTURE-NAME-P need
                               ;; not work on raw characters
                               (clim:event-matches-gesture-name-p gesture g))))
                    (or (some #'gesture-matches-p activation-gestures)
                        (some #'gesture-matches-p delimiter-gestures))))))
          (t nil))))

(defun invoke-handle-empty-input
    (stream input-continuation handler-continuation)
  (unless (clim:input-editing-stream-p stream)
    (return-from invoke-handle-empty-input (funcall input-continuation)))
  (let ((begin-scan-pointer (clim:stream-scan-pointer stream))
        (activation-gestures clim:*activation-gestures*)
        (delimiter-gestures clim:*delimiter-gestures*))
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
       (clim:view (clim:stream-default-view stream))
       (default nil defaultp) (default-type nil default-type-p)
       provide-default insert-default (clim:replace-input t)
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
  (unless (or activationsp additional-activations-p clim:*activation-gestures*)
    (setq activation-gestures clim:*standard-activation-gestures*))
  (clim:with-input-editing
      ;; this is the main change from CLIM:ACCEPT-1 -- no sensitizer.
      (stream :input-sensitizer nil)
    ;; KLUDGE: no call to CLIMI::WITH-INPUT-POSITION here, but that's
    ;; OK because we are always going to create a new editing stream
    ;; for each call to accept/accept-1-for-minibuffer, so the default
    ;; default for the BUFFER-START argument to REPLACE-INPUT is
    ;; right.
    (when (and insert-default
               (not (clim:stream-rescanning-p stream)))
      ;; Insert the default value to the input stream. It should
      ;; become fully keyboard-editable. We do not want to insert
      ;; the default if we're rescanning, only during initial
      ;; setup.
      (clim:presentation-replace-input stream default default-type clim:view))
    (clim:with-input-context (type)
        (object object-type clim:event options)
        (clim:with-activation-gestures ((if additional-activations-p
                                       additional-activation-gestures
                                       activation-gestures)
                                   :override activationsp)
          (clim:with-delimiter-gestures ((if additional-delimiters-p
                                        additional-delimiter-gestures
                                        delimiter-gestures)
                                    :override delimitersp)
            (let ((accept-results nil))
              (climi::handle-empty-input
                  (stream)
                  (setq accept-results
                        (multiple-value-list
                         (if defaultp
                             (clim:funcall-presentation-generic-function
                              clim:accept type stream clim:view
                              :default default :default-type default-type)
                             (clim:funcall-presentation-generic-function
                              clim:accept type stream clim:view))))
                ;; User entered activation or delimiter gesture
                ;; without any input.
                (if defaultp
                    (clim:presentation-replace-input
                     stream default default-type clim:view :rescan nil)
                    (clim:simple-parse-error
                     "Empty input for type ~S with no supplied default"
                     type))
                (setq accept-results (list default default-type)))
              ;; Eat trailing activation gesture
              ;; XXX what about pointer gestures?
              ;; XXX and delimiter gestures?
              ;;
              ;; deleted check for *RECURSIVE-ACCEPT-P*
              (let ((ag (clim:read-gesture :stream stream :timeout 0)))
                (unless (or (null ag) (eq ag stream))
                  (unless (clim:activation-gesture-p ag)
                    (clim:unread-gesture ag :stream stream))))
              (values (car accept-results)
                      (if (cdr accept-results) (cadr accept-results) type)))))
      ;; A presentation was clicked on, or something.
      (t
       (when (and clim:replace-input
                  (getf options :echo t)
                  (not (clim:stream-rescanning-p stream)))
         (clim:presentation-replace-input
          stream object object-type clim:view :rescan nil))
       (values object object-type)))))

(defgeneric invoke-with-minibuffer-stream (minibuffer continuation))

(defmethod invoke-with-minibuffer-stream
    ((minibuffer minibuffer-pane) continuation)
  (clim:window-clear minibuffer)
  (setf (message minibuffer)
        (clim:with-new-output-record (minibuffer)
          (setf (message-time minibuffer) (get-universal-time))
          (clim:filling-output
              (minibuffer :fill-width
                          (clim:bounding-rectangle-width minibuffer))
            (funcall continuation minibuffer)))))

(defmethod invoke-with-minibuffer-stream
    ((minibuffer clim:pointer-documentation-pane) continuation)
  (funcall continuation minibuffer))

(defmethod invoke-with-minibuffer-stream ((minibuffer null) continuation)
  (declare (ignore continuation))
  nil)

(defmacro with-minibuffer-stream ((stream-symbol) &body body)
  `(invoke-with-minibuffer-stream *minibuffer*
                                  #'(lambda (,stream-symbol)
                                      ,@body)))

(setf (documentation 'with-minibuffer-stream 'function)
      (format nil "Bind `stream-symbol' to the minibuffer stream and~@
                   evaluate `body'. This macro makes sure to setup the~@
                   initial blanking of the minibuffer as well as taking~@
                   care of for how long the message should be displayed."))

(defun display-message (format-string &rest format-args)
  (with-minibuffer-stream (minibuffer)
    (apply #'format minibuffer format-string format-args)))

(setf (documentation 'display-message 'function)
      (format nil "Display a message in the minibuffer.  Composes~@
                   the string based on the `format-string' and the~@
                   `format-args'."))
