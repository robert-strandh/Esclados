(cl:in-package #:esclados)

(defvar *esclados-instance* nil)

(setf (documentation '*esclados-instance* 'variable)
      (format nil "This symbol should be bound to an ESCLADOS instance,~@
                   though any object will do, provided the proper methods~@
                   are defined. It will be used as the argument to the~@
                   various \"query\" functions defined by ESCLADOS.~@
                   For the vast majority of ESCLADOS instances,~@
                   `*esclados-instance*' will probably have the same value~@
                   as `clim:*application-frame*'."))

(defgeneric buffers (esclados)
  (:documentation "Return a list of all the buffers of the application."))

(defgeneric present-buffer (esclados)
  (:documentation "Return the current buffer of the ESCLADOS instance ESCLADOS."))

(defgeneric (setf present-buffer) (new-buffer esclados)
  (:documentation
   #.(format nil "Replace the current buffer of the ESCLADOS instance~@
                  ESCLADOS with NEW-BUFFER.")))

(defun current-buffer ()
  "Return the currently active buffer of the running ESCLADOS."
  (present-buffer *esclados-instance*))

(defun (setf current-buffer) (new-buffer)
  #.(format nil "Replace the current buffer of the current running~@
                 ESCLADOS instance with NEW-BUFFER.")
  (setf (present-buffer *esclados-instance*) new-buffer))

(defgeneric windows (esclados)
  (:documentation "Return a list of all the windows of the ESCLADOS.")
  (:method ((esclados application-frame))
    '()))

(defgeneric present-window (esclados)
  (:documentation "Return the currently active window of ESCLADOS."))

(defun current-window ()
  "Return the currently active window of the running ESCLADOS instance."
  (present-window *esclados-instance*))

(defgeneric esclados-command-table (esclados)
  (:documentation "Return command table of ESCLADOS."))

(defvar *previous-command* nil
  #.(format nil "When a command is being executed, the command~@
                 previously executed by the application."))

