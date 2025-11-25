(cl:in-package #:esclados)

(defclass example-info-pane (info-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20
   :display-function 'display-info
   :incremental-redisplay t))

(defun display-info (frame pane)
  (declare (ignore frame))
  (format pane "Pane name: ~s" (pane-name (master-pane pane))))

(defclass example-minibuffer-pane (minibuffer-pane)
  ()
  (:default-initargs
   :height 20 :max-height 20 :min-height 20))

(defclass example-pane (esclados-pane-mixin application-pane)
  ((contents :initform "hello" :accessor contents)))

(define-application-frame example (esclados-frame-mixin
                                   standard-application-frame)
  ()
  (:panes
   (window (let* ((my-pane (make-pane 'example-pane
                                      :width 900 :height 400
                                      :display-function 'display-my-pane
                                      :command-table 'global-example-table))
                  (my-info-pane (make-pane 'example-info-pane
                                           :master-pane my-pane
                                           :width 900)))
             (setf (windows *application-frame*) (list my-pane))
             (vertically ()
               (scrolling ()
                 my-pane)
               my-info-pane)))
   (minibuffer (make-pane 'example-minibuffer-pane :width 900)))
  (:layouts
   (default (vertically ()
              window
              minibuffer)))
  (:top-level (esclados-top-level)))

(defun display-my-pane (frame pane)
  (declare (ignore frame))
  (princ (contents pane) *standard-output*))

(defun example (&key (width 900) (height 400))
  "Starts up the example application"
  (let ((frame (make-application-frame
                'example
                :width width :height height)))
    (run-frame-top-level frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands and key bindings

(define-command-table global-example-table
  :inherit-from (global-table keyboard-macro-table))
