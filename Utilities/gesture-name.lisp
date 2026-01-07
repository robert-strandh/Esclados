(cl:in-package #:esclados-utils)

;;; This definition of gesture-name is not great.  It gives a strange
;;; result for gestures like (<char> :control).

(defgeneric gesture-name (gesture))

(defmethod gesture-name ((char character))
  (if (and (graphic-char-p char)
           (not (char= char #\Space)))
      (string char)
      (or (char-name char)
          char)))

(defun translate-name-and-modifiers (key-name modifiers)
  (with-output-to-string (s)
    (loop for (modifier name) on (list
                                  ;;(+alt-key+ "A-")
                                  clim:+hyper-key+ "H-"
                                  clim:+super-key+ "s-"
                                  clim:+meta-key+ "M-"
                                  clim:+control-key+ "C-")
          by #'cddr
          when (plusp (logand modifier modifiers))
            do (princ name s))
    (princ (if (typep key-name 'character)
               (gesture-name key-name)
               key-name) s)))

(defmethod gesture-name ((ev clim:keyboard-event))
  (let ((key-name (clim:keyboard-event-key-name ev))
        (modifiers (clim:event-modifier-state ev)))
    (translate-name-and-modifiers key-name modifiers)))

(defmethod gesture-name ((gesture list))
  (cond ((eq (car gesture) :keyboard)
         (translate-name-and-modifiers (second gesture) (third gesture)))
        ;; Assume `gesture' is a list of gestures.
        (t (format nil "~{~A~#[~; ~; ~]~}"
                   (mapcar #'gesture-name gesture)))))

;;; GESTURE might be something like :CONTROL or :META.
(defmethod gesture-name ((gesture symbol))
  gesture)
