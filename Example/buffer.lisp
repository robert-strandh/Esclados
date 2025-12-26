(cl:in-package #:esclados-example)

(defgeneric contentents (buffer))

(defgeneric (setf contents) (new-contents buffer))

(defgeneric cursor (buffer))

(defgeneric (setf cursor) (new-cursor buffer))

(defclass buffer ()
  ((%contents :initform "" :accessor contents)
   (%cursor :initform 0 :accessor cursor)))

(defun insert-character (character buffer)
  (with-accessors ((contents contents) (cursor cursor)) buffer
    (setf contents
          (concatenate
           'string
           (subseq contents 0 cursor)
           (string character)
           (subseq contents cursor)))
    (incf cursor)))

(defun delete-character (buffer)
  (with-accessors ((contents contents) (cursor cursor)) buffer
    (setf contents
          (concatenate
           'string
           (subseq contents 0 cursor)
           (subseq contents (1+ cursor))))))

(defun forward-character (buffer)
  (incf (cursor buffer)))

(defun backward-character (buffer)
  (decf (cursor buffer)))
