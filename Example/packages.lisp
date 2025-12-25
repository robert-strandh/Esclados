(cl:in-package #:common-lisp-user)

(defpackage #:esclados-example
  (:use #:common-lisp #:esclados)
  (:local-nicknames (#:mini #:esclados-minibuffer))
  (:export #:example))
