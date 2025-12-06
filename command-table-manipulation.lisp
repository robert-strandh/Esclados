(cl:in-package #:esclados)

;;; Helper to avoid calling find-keystroke-item at load time. In Classic CLIM
;;; that function doesn't work if not connected to a port.

(defun compare-gestures (g1 g2)
  (and (eql (car g1) (car g2))
       (eql (apply #'clim:make-modifier-state (cdr g1))
            (apply #'clim:make-modifier-state (cdr g2)))))

(defun find-gesture-item (table gesture)
  (clim:map-over-command-table-keystrokes
   (lambda (name gest item)
     (declare (ignore name))
     (when (compare-gestures gesture gest)
       (return-from find-gesture-item item)))
   table)
  nil)

(defun ensure-subtable (table gesture)
  ;; Not having a sheet here is not conforming.
  (let* ((modifier-state (apply #'clim:make-modifier-state (cdr gesture)))
         (clim:event (make-instance 'clim:key-press-event
                               :sheet nil
                               :x 0 :y 0
                               :key-name nil
                               :key-character (car gesture)
                               :modifier-state modifier-state))
         (item (clim:find-keystroke-item clim:event table :errorp nil)))
    (when (or (null item) (not (eq (clim:command-menu-item-type item) :menu)))
      (let ((name (gensym)))
        (clim:make-command-table name :errorp nil)
        (clim:add-menu-item-to-command-table table (symbol-name name)
                                        :menu name
                                        :keystroke gesture)))
    (clim:command-menu-item-value
     (clim:find-keystroke-item clim:event table :errorp nil))))

(defun set-key (clim:command table gestures)
  (unless (consp clim:command)
    (setf clim:command (list clim:command)))
  (let ((gesture (car gestures)))
    (cond ((null (cdr gestures))
           (clim:add-keystroke-to-command-table
            table gesture :command clim:command :errorp nil)
           (when (and (listp gesture)
                      (find :meta gesture))
             ;; KLUDGE: this is a workaround for poor McCLIM
             ;; behaviour; really this canonization should happen in
             ;; McCLIM's input layer.
             (set-key clim:command table
                      (list (list :escape)
                            (let ((esc-list (remove :meta gesture)))
                              (if (and (= (length esc-list) 2)
                                       (find :shift esc-list))
                                  (remove :shift esc-list)
                                  esc-list))))))
          (t (set-key clim:command
                      (ensure-subtable table gesture)
                      (cdr gestures))))))
