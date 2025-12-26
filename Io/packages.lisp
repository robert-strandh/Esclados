(cl:in-package #:common-lisp-user)

(defpackage #:esclados-io
  (:use :clim-lisp)
  (:local-nicknames (#:utils #:esclados-utils)
                    (#:buf #:esclados-buffer)
                    (#:mini #:esclados-minibuffer)
                    (#:tbl #:esclados-command-table-manipulation))
  (:export #:frame-find-file #:find-file
           #:frame-find-file-read-only #:find-file-read-only
           #:frame-set-visited-file-name #:set-visited-filename
           #:check-buffer-writability
           #:frame-save-buffer #:save-buffer
           #:frame-write-buffer #:write-buffer
           #:buffer-writing-error #:buffer #:filepath
           #:filepath-is-directory
           #:io-table #:io-menu-table
           #:com-find-file #:com-find-file-read-only
           #:com-read-only #:com-set-visited-file-name
           #:com-save-buffer #:com-write-buffer))
