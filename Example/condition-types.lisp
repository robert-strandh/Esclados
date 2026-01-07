(cl:in-package #:esclados-example)

(define-condition beginning-of-buffer ()
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream
                     "An attempt was made to move backward, or to erase~@
                      the element before the first one in the buffer,~@
                      whereas the cursor is already at the beginning of~@
                      the buffer"))))
