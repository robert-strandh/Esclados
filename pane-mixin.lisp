(cl:in-package #:esclados)

(defclass pane-mixin ()
  (;; allows a certain number of commands to have some minimal memory
   (%previous-command
    :initform nil
    :accessor previous-command)
   (%command-table
    :initarg :command-table
    :accessor esclados-command-table)))

(defmethod previous-command ((pane clim:pane))
  nil)
