(cl:in-package #:asdf-user)

;;; Copyright (c) 2005-2006, 2025 Robert Strandh (robert.strandh@gmail.com)
;;; Copyright (c) 2006, Troels Henriksen (athas@sigkill.dk)

(defsystem "esclados"
  :depends-on ("clim-core"
               "esclados-utilities"
               "esclados-minibuffer"
               "esclados-command-processing"
               "esclados-buffer"
               "esclados-command-table-manipulation"
               "esclados-pane"
               "esclados-frame"
               "esclados-top-level"
               "esclados-command-parser")
  :serial t
  :components
  ((:file "packages")
   (:file "standard-key-bindings")
   (:file "keyboard-macros")))
