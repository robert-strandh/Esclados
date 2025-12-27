(cl:in-package #:esclados-frame)

(defclass frame-mixin (cmd:command-processor)
  ((%windows :accessor windows)))

(defmethod present-buffer ((esclados frame-mixin))
  (first (buffers esclados)))

(defmethod present-window ((esclados frame-mixin))
  (first (windows esclados)))

(defmethod cmd:command-table ((frame frame-mixin))
  (find-applicable-command-table frame))

;; Defaults for non-ESCLADOS-frames.
(defmethod cmd:recordingp ((frame clim:application-frame))
  nil)

(defmethod cmd:executingp ((frame clim:application-frame))
  nil)

(defmethod cmd:recorded-keys ((frame clim:application-frame))
  nil)

(defmethod cmd:remaining-keys ((frame clim:application-frame))
  nil)

(defmethod minibuffer ((clim:application-frame frame-mixin))
  (clim:frame-standard-input clim:application-frame))

(defmethod clim:redisplay-frame-panes :around
    ((frame frame-mixin) &key force-p)
  (declare (ignore force-p))
  (when (null (cmd:remaining-keys frame))
    (setf (cmd:executingp frame) nil)
    (call-next-method)))

(defmethod clim:execute-frame-command :after ((frame frame-mixin) command)
  ;; FIXME: I'm not sure that we want to do this for commands sent
  ;; from other threads; we almost certainly don't want to do it twice
  ;; in such cases...
  (setf (pane:previous-command (present-window frame)) command))

(defmethod clim:execute-frame-command :around ((frame frame-mixin) command)
  (declare (ignore command))
  (call-next-method)
  (when (eq frame clim:*application-frame*)
    (clim:redisplay-frame-panes frame)))

(defgeneric find-applicable-command-table (frame)
  (:documentation "Return the command table object that commands
on `frame' should be found in."))

(defmethod find-applicable-command-table ((frame frame-mixin))
  (command-table (car (windows frame))))

;;; For convenience.

(define-symbol-macro applicable-command-table
    (find-applicable-command-table clim:*application-frame*))
