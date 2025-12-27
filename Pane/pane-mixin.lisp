(cl:in-package #:esclados-pane)

(defgeneric previous-command (pane-mixin))

(defgeneric (setf previous-command) (command pane-mixin))

(defclass pane-mixin ()
  (;; allows a certain number of commands to have some minimal memory
   (%previous-command
    :initform nil
    :accessor previous-command)
   (%command-table
    :initarg :command-table
    :accessor command-table)))

(defmethod previous-command ((pane clim:pane))
  nil)
