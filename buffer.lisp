(in-package :esclados-buffer)

(defgeneric frame-make-buffer-from-stream (application-frame stream)
  (:documentation "Create a fresh buffer by reading the external
representation from STREAM"))

(defun make-buffer-from-stream (stream)
  "Create a fresh buffer by reading the external representation
from STREAM"
  (frame-make-buffer-from-stream *application-frame* stream))

(defgeneric frame-make-new-buffer (application-frame &key &allow-other-keys)
  (:documentation "Create a empty buffer for the application frame."))

(defun make-new-buffer (&rest args &key &allow-other-keys)
  "Create a empty buffer for the current frame."
  (apply #'frame-make-new-buffer *application-frame* args))

(defgeneric frame-save-buffer-to-stream (application-frame buffer stream)
  (:documentation "Save the entire BUFFER to STREAM in the appropriate
external representation"))

(defun save-buffer-to-stream (buffer stream)
  "Save the entire BUFFER to STREAM in the appropriate external
representation"
  (frame-save-buffer-to-stream *application-frame* buffer stream))

(defclass esclados-buffer-mixin (name-mixin)
  ((%filepath :initform nil :accessor filepath)
   (%needs-saving :initform nil :accessor needs-saving)
   (%file-write-time :initform nil :accessor file-write-time)
   (%file-saved-p :initform nil :accessor file-saved-p)
   (%read-only-p :initform nil :accessor read-only-p))
  (:default-initargs :name "*scratch*"))
