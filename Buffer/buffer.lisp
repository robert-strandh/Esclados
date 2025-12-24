(cl:in-package #:esclados-buffer)

(defgeneric frame-make-buffer-from-stream (application-frame stream))

(setf (documentation 'frame-make-buffer-from-stream 'function)
      (format nil "Create a fresh buffer by reading the external~@
                   representation from STREAM"))

(defun make-buffer-from-stream (stream)
  (frame-make-buffer-from-stream clim:*application-frame* stream))

(setf (documentation 'make-buffer-from-stream 'function)
      (format nil "Create a fresh buffer by reading the external~@
                   representation from STREAM"))

(defgeneric frame-make-new-buffer (application-frame &key &allow-other-keys))

(setf (documentation 'frame-make-new-buffer 'function)
      (format nil "Create a empty buffer for the application frame."))

(defun make-new-buffer (&rest args &key &allow-other-keys)
  (apply #'frame-make-new-buffer clim:*application-frame* args))

(setf (documentation 'make-new-buffer 'function)
      (format nil "Create a empty buffer for the current frame."))

(defgeneric frame-save-buffer-to-stream (application-frame buffer stream))

(setf (documentation 'frame-save-buffer-to-stream 'function)
      (format nil "Save the entire BUFFER to STREAM in the appropriate~@
                   external representation"))

(defun save-buffer-to-stream (buffer stream)
  (frame-save-buffer-to-stream clim:*application-frame* buffer stream))

(setf (documentation 'save-buffer-to-stream 'function)
      (format nil "Save the entire BUFFER to STREAM in the~@
                   appropriate external representation"))

(defclass buffer-mixin (utils:name-mixin)
  ((%filepath :initform nil :accessor filepath)
   (%needs-saving :initform nil :accessor needs-saving)
   (%file-write-time :initform nil :accessor file-write-time)
   (%file-saved-p :initform nil :accessor file-saved-p)
   (%read-only-p :initform nil :accessor read-only-p))
  (:default-initargs :name "*scratch*"))
