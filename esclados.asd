(cl:in-package #:asdf-user)

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
               "esclados-command-parser"
               "esclados-standard-key-bindings"
               "esclados-keyboard-macros"
               "esclados-info-pane")
  :license "BSD 2-clause, see file LICENSE.text"
  :serial t)
