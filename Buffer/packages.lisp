(cl:in-package #:common-lisp-user)

(defpackage #:esclados-buffer
  (:use :clim-lisp)
  (:local-nicknames (#:utils #:esclados-utils))
  (:export #:frame-make-buffer-from-stream #:make-buffer-from-stream
           #:frame-save-buffer-to-stream #:save-buffer-to-stream
           #:filepath #:name #:needs-saving #:file-write-time #:file-saved-p
           #:buffer-mixin
           #:frame-make-new-buffer #:make-new-buffer
           #:read-only-p))
