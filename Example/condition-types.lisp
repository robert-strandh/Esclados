(cl:in-package #:esclados-example)

(define-condition beginning-of-buffer ()
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "An attempt was made to move backward, whereas~@
                      the cursor is already at the beginning of the~@
                      buffer"))))
