(cl:in-package #:esclados)

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

;;; For convenience.

(define-symbol-macro applicable-command-table
    (find-applicable-command-table clim:*application-frame*))


  
