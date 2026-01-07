(cl:in-package #:esclados-example)

(defgeneric contentents (buffer))

(defgeneric (setf contents) (new-contents buffer))

(defgeneric cursor (buffer))

(defgeneric (setf cursor) (new-cursor buffer))

(defclass buffer ()
  ((%contents
    :initform (make-array 10
                          :element-type 'character
                          :adjustable t
                          :fill-pointer 0)
    :reader contents)
   (%cursor :initform 0 :accessor cursor)))

(defun insert-character (character buffer)
  (with-accessors ((contents contents) (cursor cursor)) buffer
    (vector-push-extend #\Space contents 10)
    (replace contents contents :start1 (1+ cursor) :start2 cursor)
    (setf (char contents cursor) character)
    (incf cursor)))

(defun delete-character (buffer)
  (with-accessors ((contents contents) (cursor cursor)) buffer
    (replace contents contents :start1 cursor :start2 (1+ cursor))
    (decf (fill-pointer contents))))

(defun forward-character (buffer)
  (incf (cursor buffer)))

(defun backward-character (buffer)
  (decf (cursor buffer)))
