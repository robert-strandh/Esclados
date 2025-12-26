(cl:in-package #:esclados-event-handling)

(defgeneric convert-to-gesture (clim:event))

(setf (documentation 'convert-to-gesture 'function)
      (format nil "Convert `event' (which must be an input event) to~@
                   a CLIM gesture, or NIL, if this is not possible."))

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
