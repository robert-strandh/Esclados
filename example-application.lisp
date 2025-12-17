(cl:in-package #:esclados)

(defclass example-info-pane (info-pane)
  ()
  (:default-initargs
   :display-function 'display-info
   :incremental-redisplay t))

(defun display-info (frame pane)
  (declare (ignore frame))
  (format pane "Pane name: ~s" (clim:pane-name (master-pane pane))))

(defclass example-minibuffer-pane (mini:minibuffer-pane)
  ())

(defclass example-pane (pane-mixin clim:application-pane)
  ((contents :initform "hello" :accessor contents)))

(clim:define-application-frame example
    (esclados-frame-mixin clim:standard-application-frame)
  ()
  (:panes
   (window (let* ((my-pane (clim:make-pane 'example-pane
                                      :width 900 :height 400
                                      :display-function 'display-my-pane
                                      :name "Example"
                                      :command-table 'global-example-table))
                  (my-info-pane (clim:make-pane 'example-info-pane
                                           :master-pane my-pane
                                           :width 900))
                  (minibuffer (clim:make-pane 'example-minibuffer-pane
                                         :width 900)))
             (setf (windows clim:*application-frame*) (list my-pane))
             (clim:vertically ()
               (clim:scrolling ()
                 my-pane)
               (20 my-info-pane)
               (20 minibuffer)))))
  (:layouts
   (default window))
  (:top-level (top-level)))

(defun display-my-pane (frame pane)
  (declare (ignore frame))
  (princ (contents pane) *standard-output*))

(defun example (&key (width 900) (height 400))
  "Starts up the example application"
  (let ((frame (clim:make-application-frame
                'example
                :width width :height height)))
    (clim:run-frame-top-level frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Commands and key bindings

(clim:define-command-table global-example-table
  :inherit-from (global-table keyboard-macro-table))
