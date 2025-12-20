(cl:in-package #:common-lisp-user)

(defpackage #:esclados-command-processor
  (:use #:common-lisp)
  (:local-nicknames (#:mini #:esclados-minibuffer)
                    (#:clime #:clim-extensions))
  (:export
   #:command-processor
   #:command-loop-command-processor
   #:overriding-handler
   #:recordingp
   #:executingp
   #:recorded-keys
   #:remaining-keys
   #:command-table
   #:gestures
   #:*current-gesture*
   #:*command-processor*
   #:*abort-gestures*
   #:read-gesture
   #:process-gestures-or-command
   #:process-gesture
   #:find-gestures
   #:find-gestures-with-inheritance
   #:unbound-gesture-sequence))

