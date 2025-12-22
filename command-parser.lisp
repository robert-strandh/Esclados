(cl:in-package #:esclados)

;;; There is an ambiguity over what to do for parsing partial commands
;;; with certain values filled in, as might occur for keyboard
;;; shortcuts.  Either the supplied arguments should be treated as
;;; gospel and not even mentioned to the user, as we do now; or they
;;; should be treated as the default, but the user should be prompted
;;; to confirm, as we used to do.
(defun parse-one-arg (stream name ptype accept-args)
  (declare (ignore name))
  ;; this conditional doesn't feel entirely happy.  The issue is that
  ;; we could be called either recursively from an outer call to
  ;; (accept 'command), in which case we want our inner accept to
  ;; occur on the minibuffer stream not the input-editing-stream, or
  ;; from the toplevel when handed a partial command.  Maybe the
  ;; toplevel should establish an input editing context for partial
  ;; commands anyway?  Then ESCLADOS-PARSE-ONE-ARG would always be called
  ;; with an input-editing-stream.
  (let ((stream (if (clim:encapsulating-stream-p stream)
                    (clim:encapsulating-stream-stream stream)
                    stream)))
    (apply #'clim:accept (eval ptype)
           :stream stream
           ;; This is fucking nuts.  FIXME: the clim spec says
           ;; ":GESTURE is not evaluated at all".  Um, but how are you
           ;; meant to tell if a keyword argument is :GESTURE, then?
           ;; The following does not actually allow variable keys:
           ;; anyone who writes (DEFINE-COMMAND FOO ((BAR 'PATHNAME
           ;; *RANDOM-ARG* ""))) and expects it to work deserves to
           ;; lose.
           ;;
           ;; FIXME: this will do the wrong thing on malformed accept
           ;; arguments, such improper lists or those with an odd
           ;; number of keyword arguments.  I doubt that
           ;; DEFINE-COMMAND is checking the syntax, so we probably
           ;; should.
           (loop for (key val) on accept-args by #'cddr
                 unless (eq key :gesture)
                 collect key and collect (eval val)))))

(defun command-parser (clim:command-table stream)
  (let ((clim:command-name nil))
    (flet ((maybe-clear-input ()
             (let ((gesture (clim:read-gesture :stream stream 
                                               :peek-p t :timeout 0)))
               (when (and gesture (or (clim:delimiter-gesture-p gesture)
                                      (clim:activation-gesture-p gesture)))
                 (clim:read-gesture :stream stream)))))
      (clim:with-delimiter-gestures
          (clim:*command-name-delimiters* :override t)
        ;; While reading the command name we want use the history of
        ;; the (accept 'command ...) that's calling this function.
        ;;
        ;; FIXME: does this :history nil actually achieve the above?
        (setq clim:command-name
              (clim:accept
               `(clim:command-name :command-table ,clim:command-table)
               :stream (clim:encapsulating-stream-stream stream)
               :prompt *extended-command-prompt*
               :prompt-mode :raw :history nil))
        (maybe-clear-input))
      (clim:with-delimiter-gestures
          (clim:*command-argument-delimiters* :override t)
        ;; FIXME, except we can't: use of CLIM-INTERNALS.
        (let* ((info (gethash clim:command-name climi::*command-parser-table*))
               (required-args (climi::required-args info))
               (keyword-args (climi::keyword-args info)))
          (declare (ignore keyword-args))
          (let (result)
            ;; only required args for now.
            (dolist (arg required-args (cons clim:command-name (nreverse result)))
              (destructuring-bind (name ptype &rest args) arg
                (push (parse-one-arg stream name ptype args) result)
                (maybe-clear-input)))))))))

(defun esclados-partial-command-parser
    (clim:command-table stream clim:command position
     &optional numeric-argument)
  (declare (ignore clim:command-table position))
  (let ((clim:command-name (car clim:command))
	(command-args (cdr clim:command)))
    (flet ((maybe-clear-input ()
             (let ((gesture (clim:read-gesture :stream stream 
                                               :peek-p t :timeout 0)))
               (when (and gesture (or (clim:delimiter-gesture-p gesture)
                                      (clim:activation-gesture-p gesture)))
                 (clim:read-gesture :stream stream)))))
      (clim:with-delimiter-gestures
          (clim:*command-argument-delimiters* :override t)
        ;; FIXME, except we can't: use of CLIM-INTERNALS.
        (let ((info (gethash clim:command-name climi::*command-parser-table*)))
          (if (null info)
              ;; `command' is not a real command! Well, we can still
              ;; replace numeric argument markers.
              (clim:substitute-numeric-argument-marker clim:command numeric-argument)
              (let ((required-args (climi::required-args info))
                    (keyword-args (climi::keyword-args info)))
                ;; keyword arguments not yet supported
                (declare (ignore keyword-args))
                (let (result arg-parsed)
                  ;; only required args for now.
                  (do* ((required-args required-args (cdr required-args))
                        (arg (car required-args) (car required-args))
                        (command-args command-args (cdr command-args))
                        (command-arg (car command-args) (car command-args)))
                       ((null required-args) (cons clim:command-name (nreverse result)))
                    (destructuring-bind (name ptype &rest args) arg
                      (push (cond ((eq command-arg clim:*unsupplied-argument-marker*)
                                   (setf arg-parsed t)
                                   (parse-one-arg stream name ptype args))
                                  ((eq command-arg clim:*numeric-argument-marker*)
                                   (or numeric-argument (getf args :default)))
                                  (t (eval command-arg)))
                            result)
                      (when arg-parsed
                        (maybe-clear-input))))))))))))
