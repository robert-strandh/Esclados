(in-package :esclados-io)

(defgeneric frame-find-file (application-frame file-path))

(setf (documentation 'frame-find-file 'function)
      (format nil "If a buffer with the file-path already exists,~@
                   return it, else if a file with the right name exists,~@
                   return a fresh buffer created from the file, else return~@
                   a new empty buffer having the associated file name."))

(defgeneric frame-find-file-read-only (application-frame file-path))

(defgeneric frame-set-visited-file-name (application-frame filepath buffer))

(defgeneric check-buffer-writability (application-frame filepath buffer))

(setf (documentation 'check-buffer-writability 'function)
      (format nil "Check that `buffer' can be written to `filepath',~@
                   which can be an arbitrary pathname. If there is a~@
                   problem, an error that is a subclass of~@
                   `buffer-writing-error'should be signaled."))

(defgeneric frame-save-buffer (application-frame buffer))

(defgeneric frame-write-buffer (application-frame filepath buffer))

(define-condition buffer-writing-error (error)
  (;; This slot contains the buffer that was attempted to be written
   ;; when this error was signaled.
   (%buffer :reader buffer
            :initarg :buffer
            :initform (error "A buffer must be provided"))
   ;; This slot contains the filepath that the buffer was attempted
   ;; to be saved to when this error was signaled.
   (%filepath :reader filepath
              :initarg :filepath
              :initform (error "A filepath must be provided")))
  (:report (lambda (condition stream)
             (format stream "~A could not be saved to ~A"
                     (utils:name (buffer condition)) (filepath condition)))))

(setf (documentation 'buffer-writing-error 'type)
      (format nil "An error that is a subclass of this class will be~@
                   signaled when a buffer is attempted saved to a file,~@
                   but something goes wrong. Not all error cases will~@
                   result in the signaling of a `buffer-writing-error',~@
                   but some defined cases will."))

(define-condition filepath-is-directory (buffer-writing-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Cannot save buffer ~A to just a directory"
                     (utils:name (buffer condition))))))

(setf (documentation 'filepath-is-directory 'type)
      (format nil "This error is signaled when a buffer is attempted~@
                   saved to a directory."))

(defun filepath-is-directory (buffer filepath)
  (error 'filepath-is-directory :buffer buffer :filepath filepath))

(setf (documentation 'filepath-is-directory 'function)
      (format nil "Signal an error of type `filepath-is-directory' with~@
                   the buffer `buffer' and the filepath `filepath'."))

(defun find-file (file-path)
  (frame-find-file clim:*application-frame* file-path))

(defun find-file-read-only (file-path)
  (frame-find-file-read-only clim:*application-frame* file-path))

(defun set-visited-file-name (filepath buffer)
  (frame-set-visited-file-name clim:*application-frame* filepath buffer))

(defun save-buffer (buffer)
  (frame-save-buffer clim:*application-frame* buffer))

(defun write-buffer (filepath buffer)
  (frame-write-buffer clim:*application-frame* filepath buffer))

(clim:make-command-table 'io-table :errorp nil)

;;; Adapted from cl-fad/PCL
(defun directory-pathname-p (pathspec)
  "Returns NIL if PATHSPEC does not designate a directory."
  (let ((name (pathname-name pathspec))
        (type (pathname-type pathspec)))
    (and (or (null name) (eql name :unspecific))
         (or (null type) (eql type :unspecific)))))

(defun filepath-filename (pathname)
  (if (null (pathname-type pathname))
      (pathname-name pathname)
      (concatenate 'string (pathname-name pathname)
                   "." (pathname-type pathname))))

(defmethod frame-find-file (application-frame filepath)
  (declare (ignore application-frame))
  (cond ((null filepath)
         (display-message "No file name given.")
         (clim:beep))
        ((directory-pathname-p filepath)
         (display-message "~A is a directory name." filepath)
         (clim:beep))
        (t
         (or (find filepath (buffers clim:*application-frame*)
                   :key #'filepath :test #'equal)
             (let ((buffer (if (probe-file filepath)
                               (with-open-file (stream filepath :direction :input)
                                 (buf:make-buffer-from-stream stream))
                               (buf:make-new-buffer))))
               (setf (buf:filepath buffer) filepath
                     (utils:name buffer) (filepath-filename filepath)
                     (buf:needs-saving buffer) nil)
               buffer)))))

(defun directory-of-current-buffer ()
  "Extract the directory part of the filepath to the file in the current buffer.
   If the current buffer does not have a filepath, the path to
   the user's home directory will be returned."
  (make-pathname
   :directory
   (pathname-directory
    (or (and (current-buffer)
             (buf:filepath (current-buffer)))
        (user-homedir-pathname)))))

(clim:define-command (com-find-file :name t :command-table io-table) 
    ((filepath 'pathname
               :prompt "Find File: "
               :prompt-mode :raw
               :default (directory-of-current-buffer)
               :default-type 'pathname
               :insert-default t))
  "Prompt for a filename then edit that file.
If a buffer is already visiting that file, switch to that
buffer. Does not create a file if the filename given does not
name an existing file."
  (handler-case (find-file filepath)
    (file-error (e)
      (display-message "~A" e))))

(set-key `(com-find-file ,clim:*unsupplied-argument-marker*)
         'io-table '((#\x :control) (#\f :control)))

(defmethod frame-find-file-read-only (application-frame filepath)
  (declare (ignore application-frame))
  (cond ((null filepath)
         (display-message "No file name given.")
         (clim:beep))
        ((directory-pathname-p filepath)
         (display-message "~A is a directory name." filepath)
         (clim:beep))
        (t
         (or (find filepath (buffers clim:*application-frame*)
                   :key #'filepath :test #'equal)
             (if (probe-file filepath)
                 (with-open-file (stream filepath :direction :input)
                   (let ((buffer (buf:make-buffer-from-stream stream)))
                     (setf (buf:filepath buffer) filepath
                           (utils:name buffer) (filepath-filename filepath)
                           (buf:read-only-p buffer) t
                           (buf:needs-saving buffer) nil)))
                 (progn
                   (display-message "No such file: ~A" filepath)
                   (clim:beep)
                   nil))))))

(clim:define-command (com-find-file-read-only :name t :command-table io-table)
    ((filepath 'pathname
               :prompt "Find File read-only: "
               :prompt-mode :raw
               :default (directory-of-current-buffer)
               :default-type 'pathname
               :insert-default t))
  "Prompt for a filename then open that file readonly.
If a buffer is already visiting that file, switch to that
buffer. If the filename given does not name an existing file,
signal an error."
  (find-file-read-only filepath))

(set-key `(com-find-file-read-only ,clim:*unsupplied-argument-marker*)
         'io-table '((#\x :control) (#\r :control)))

(clim:define-command (com-read-only :name t :command-table io-table)
    ()
  "Toggle the readonly status of the current buffer.
When a buffer is readonly, attempts to change the contents of the
buffer signal an error."
  (let ((buffer (current-buffer)))
    (setf (buf:read-only-p buffer) (not (buf:read-only-p buffer)))))

(set-key 'com-read-only 'io-table '((#\x :control) (#\q :control)))

(defmethod frame-set-visited-file-name (application-frame filepath buffer)
  (declare (ignore application-frame))
  (setf (buf:filepath buffer) filepath
        (utils:name buffer) (filepath-filename filepath)
        (buf:needs-saving buffer) t))

(clim:define-command (com-set-visited-file-name :name t :command-table io-table)
    ((filename 'pathname :prompt "New filename: "
               :prompt-mode :raw
               :default (directory-of-current-buffer)
               :insert-default t
               :default-type 'pathname
               :insert-default t))
    "Prompt for a new filename for the current buffer.
The next time the buffer is saved it will be saved to a file with
that filename."
  (set-visited-file-name filename (current-buffer)))

(defmethod check-buffer-writability (application-frame (filepath pathname)
                                     (buffer buf:esclados-buffer-mixin))
  (declare (ignore application-frame))
  ;; Cannot write to a directory.
  (when (directory-pathname-p filepath)
    (filepath-is-directory buffer filepath)))

(defun extract-version-number (pathname)
  "Extracts the emacs-style version-number from a pathname."
  (let* ((type (pathname-type pathname))
	 (length (length type)))
    (when (and (> length 2) (char= (char type (1- length)) #\~))
      (let ((tilde (position #\~ type :from-end t :end (- length 2))))
	(when tilde
	  (parse-integer type :start (1+ tilde) :junk-allowed t))))))

(defun version-number (pathname)
  "Return the number of the highest versioned backup of PATHNAME
or 0 if there is no versioned backup. Looks for name.type~X~,
returns highest X."
  (let* ((wildpath (merge-pathnames (make-pathname :type :wild) pathname))
	 (possibilities (directory wildpath)))
    (loop for possibility in possibilities
	  for version = (extract-version-number possibility) 
	  if (numberp version)
	    maximize version into max
	  finally (return max))))

(defun check-file-times (buffer filepath question answer)
  "Return NIL if filepath newer than buffer and user doesn't want
to overwrite."
  (let ((f-w-d (and (probe-file filepath) (file-write-date filepath)))
	(f-w-t (buf:file-write-time buffer)))
    (if (and f-w-d f-w-t (> f-w-d f-w-t))
	(if (clim:accept 'boolean
		    :prompt (format nil "File has changed on disk. ~a anyway?"
				    question))
	    t
	    (progn (display-message "~a not ~a" filepath answer)
		   nil))
	t)))

(defmethod frame-save-buffer (application-frame buffer)
  (let ((filepath (or (buf:filepath buffer)
                      (clim:accept 'pathname :prompt "Save Buffer to File"))))
    (check-buffer-writability application-frame filepath buffer)
    (unless (check-file-times buffer filepath "Overwrite" "written")
      (return-from frame-save-buffer))
    (when (and (probe-file filepath) (not (buf:file-saved-p buffer)))
      (let ((backup-name (pathname-name filepath))
            (backup-type (format nil "~A~~~D~~"
                                 (pathname-type filepath)
                                 (1+ (version-number filepath)))))
        (rename-file filepath (make-pathname :name backup-name
                                             :type backup-type))))
    (with-open-file (stream filepath :direction :output :if-exists :supersede)
      (buf:save-buffer-to-stream buffer stream))
    (setf (buf:filepath buffer) filepath
          (buf:file-write-time buffer) (file-write-date filepath)
          (utils:name buffer) (filepath-filename filepath))
    (display-message "Wrote: ~a" (buf:filepath buffer))
    (setf (buf:needs-saving buffer) nil)))

(clim:define-command (com-save-buffer :name t :command-table io-table) ()
  "Write the contents of the buffer to a file.
If there is filename associated with the buffer, write to that
file, replacing its contents. If not, prompt for a filename."
  (let ((buffer (current-buffer)))
    (if (null (buf:filepath buffer))
        (com-write-buffer (clim:accept 'pathname :prompt "Write Buffer to File: "
                                            :prompt-mode :raw
                                            :default (directory-of-current-buffer) :insert-default t
                                            :default-type 'pathname))
        (if (buf:needs-saving buffer)
            (handler-case (save-buffer buffer)
              ((or buffer-writing-error file-error) (e)
                (display-message "~A" e)))
            (display-message "No changes need to be saved from ~a"
                             (utils:name buffer))))))

(set-key 'com-save-buffer 'io-table '((#\x :control) (#\s :control)))

(defmethod frame-write-buffer (application-frame filepath buffer)
  (check-buffer-writability application-frame filepath buffer)
  (with-open-file (stream filepath :direction :output :if-exists :supersede)
    (buf:save-buffer-to-stream buffer stream))
  (setf (buf:filepath buffer) filepath
        (utils:name buffer) (filepath-filename filepath)
        (buf:needs-saving buffer) nil)
  (display-message "Wrote: ~a" (buf:filepath buffer)))

(clim:define-command (com-write-buffer :name t :command-table io-table) 
    ((filepath 'pathname :prompt "Write Buffer to File: " :prompt-mode :raw
               :default (directory-of-current-buffer) :insert-default t
               :default-type 'pathname))
    "Prompt for a filename and write the current buffer to it.
Changes the file visted by the buffer to the given file."
  (let ((buffer (current-buffer)))
    (handler-case (write-buffer filepath buffer)
      (buffer-writing-error (e)
        (with-minibuffer-stream (minibuffer)
          (let ((*print-escape* nil))
            (print-object e minibuffer)))))))

(set-key `(com-write-buffer ,clim:*unsupplied-argument-marker*)
         'io-table '((#\x :control) (#\w :control)))

(utils:define-menu-table esclados-io-menu-table (io-table global-table)
  `(com-find-file ,clim:*unsupplied-argument-marker*)
  `(com-find-file-read-only ,clim:*unsupplied-argument-marker*)
  'com-save-buffer
  `(com-write-buffer ,clim:*unsupplied-argument-marker*)
  `(com-set-visited-file-name ,clim:*unsupplied-argument-marker*)
  :divider
  'com-quit)
