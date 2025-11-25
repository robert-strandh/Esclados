(cl:in-package #:asdf-user)

;;; Copyright (c) 2005-2006, 2025 Robert Strandh (robert.strandh@gmail.com)
;;; Copyright (c) 2006, Troels Henriksen (athas@sigkill.dk)

(defsystem "esclados"
  :depends-on ("clim-core" "alexandria")
  :serial t
  :components
  ((:file "packages")
   (:file "utils")
   (:file "esa")
   (:file "esa-buffer")
   (:file "esa-io")
   (:file "esa-command-parser")
   (:file "keyboard-macros")
   (:file "example-application")))
