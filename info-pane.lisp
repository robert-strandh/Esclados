(cl:in-package #:esclados)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Info pane, a pane that displays some information about another pane

(defclass info-pane (application-pane)
  ((master-pane :initarg :master-pane :reader master-pane))
  (:default-initargs
   :background +gray85+))
