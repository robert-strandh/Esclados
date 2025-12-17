(cl:in-package #:asdf-user)

;;; Copyright (c) 2005-2006, 2025 Robert Strandh (robert.strandh@gmail.com)
;;; Copyright (c) 2006, Troels Henriksen (athas@sigkill.dk)

(defsystem "esclados"
  :depends-on ("clim-core")
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "query")
   (:file "info-pane")
   (:file "minibuffer-pane")
   (:file "pane-mixin")
   (:file "command-processing")
   (:file "frame-mixin")
   (:file "top-level")
   (:file "event-handling")
   (:file "command-table-manipulation")
   (:file "standard-key-bindings")
   (:file "help")
   (:file "help-commands")
   (:file "buffer")
   (:file "io")
   (:file "command-parser")
   (:file "keyboard-macros")
   (:file "example-application")))
