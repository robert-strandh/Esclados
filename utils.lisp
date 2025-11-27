(in-package :esclados-utils)

(defun unlisted (obj &optional (fn #'first))
  (if (listp obj)
      (funcall fn obj)
      obj))

(defun listed (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun list-aref (list &rest subscripts)
  (if subscripts
      (apply #'list-aref (nth (first subscripts) list)
             (rest subscripts))
      list))

(defun build-menu (command-tables &rest commands)
  (labels ((get-command-name (command)
             (or (loop for table in command-tables
                       for name = (command-line-name-for-command
                                   command table :errorp nil)
                       when name return name)
                 (error 'command-table-error
                  :format-string "Command ~A not found in any provided command table"
                  :format-arguments (list command))))
           (make-menu-entry (entry)
             (cond ((and (listp entry)
                         (eq (first entry) :menu))
                    (list (command-table-name (find-command-table (second entry)))
                     :menu (second entry)))
                   ((and (listp entry)
                         (eq (first entry) :submenu))
                    (list (second entry)
                     :menu (apply #'build-menu command-tables
                                  (cddr entry))))
                   ((eq entry :divider)
                    '(nil :divider :line))
                   (t (list (get-command-name (command-name (listed entry)))
                       :command entry)))))
    (make-command-table nil
     :inherit-from command-tables
     :menu (mapcar #'make-menu-entry commands))))

(setf (documentation 'build-menu 'function)
      (format nil "Create a command table inheriting commands from~@
                   `command-tables', which must be a list of command~@
                    table designators. The created command table will~@
                    have a menu consisting of `commands', elements of~@
                    which must be one of:~@
                    ~@
                    * A named command accessible in one of `command-tables'.~@
                      This may either be a command name, or a cons of a~@
                      command name and arguments. The command will appear~@
                      directly in the menu.~@
                    ~@
                    * A list of the symbol `:menu' and something that will~@
                      evaluate to a command table designator. This will~@
                      create a submenu showing the name and menu of the~@
                      designated command table.~@
                    ~@
                    * A list of the symbol `:submenu', a string, and a~@
                      &rest list of the same form as `commands'. This~@
                      is equivalent to `:menu' with a call to `build-menu'~@
                      with `command-tables' and the specified list as~@
                      arguments.~@
                    ~@
                    * A symbol `:divider', which will present a horizontal~@
                      divider line.~@
                    ~@
                    An error of type`command-table-error' will be signalled~@
                    if a command cannot be found in any of the provided~@
                    command tables."))

(defmacro define-menu-table (name (&rest command-tables) &body commands)
  "Define a command table with a menu named `name' and containing
`commands'. `Command-tables' must be a list of command table
designators containing the named commands that will be included
in the menu. `Commands' must have the same format as the
`commands' argument to `build-menu'. If `name' already names a
command table, the old definition will be destroyed."
  `(make-command-table ',name
    :inherit-from (list (build-menu ',command-tables
                                    ,@commands))
    :inherit-menu t
    :errorp nil))

(defclass observable-mixin ()
  ((%observers :accessor observers
               :initform '()))
  (:documentation "A mixin class that adds the capability for a
subclass to have a list of \"event subscribers\" (observers) that
can be informed via callback (the function `observer-notified')
whenever the state of the object changes. The order in which
observers will be notified is undefined."))

(defgeneric add-observer (observable observer)
  (:documentation "Add an observer to an observable object. If
the observer is already observing `observable', it will not be
added again."))

(defmethod add-observer ((observable observable-mixin) observer)
  ;; Linear in complexity, perhaps a transparent switch to a hash
  ;; table would be a good idea for large amounts of observers.
  (pushnew observer (observers observable)))

(defgeneric remove-observer (observable observer)
  (:documentation "Remove an observer from an observable
object. If observer is not in the list of observers of
`observable', nothing will happen."))

(defmethod remove-observer ((observable observable-mixin) observer)
  (setf (observers observable)
        (delete observer (observers observable))))

(defgeneric observer-notified (observer observable data)
  (:documentation "This function is called by `observable' when
its state changes on each observer that is observing
it. `Observer' is the observing object, `observable' is the
observed object. `Data' is arbitrary data that might be of
interest to `observer', it is recommended that subclasses of
`observable-mixin' specify exactly which form this data will
take, the observer protocol does not guarantee anything. It is
non-&optional so that methods may be specialised on it, if
applicable. The default method on this function is a no-op, so it
is never an error to not define a method on this generic function
for an observer.")
  (:method (observer (observable observable-mixin) data)
    (declare (ignore observer data))
    ;; Never a no-applicable-method error.
    nil))

(defgeneric notify-observers (observable &optional data-fn)
  (:documentation "Notify each observer of `observable' by
calling `observer-notified' on them. `Data-fn' will be called,
with the observer as the single argument, to obtain the `data'
argument to `observer-notified'. The default value of `data-fn'
should cause the `data' argument to be NIL."))

(defmethod notify-observers ((observable observable-mixin)
                             &optional (data-fn (constantly nil)))
  (dolist (observer (observers observable))
    (observer-notified observer observable
                       (funcall data-fn observer))))

(defclass name-mixin ()
  ((%name :accessor name
          :initarg :name
          :type string
          :documentation "The name of the named object."))
  (:documentation "A class used for defining named objects."))

#.(defparameter *subscript-generator-documentation*
    (format nil "This slot contains a function used for finding the~@
                 subscript of a `name-mixin' whenever the name is set~@
                 (including during object initialization).~@
                 ~@
                 This function will be called with the name as the~@
                 single argument."))

(defclass subscriptable-name-mixin (name-mixin)
  ((%subscript :accessor subscript
               :documentation "The subscript of the named object.")
   (%subscript-generator
    :reader subscript-generator
    :initarg :subscript-generator
    :initform (constantly 1)
    :documentation #.*subscript-generator-documentation*))
  (:documentation "A class used for defining named objects. A
facility is provided for assigning a named object a \"subscript\"
uniquely identifying the object if there are other objects of the
same name in its collection (in particular, if an editor has two
buffers with the same name)."))

(defmethod initialize-instance :after ((name-mixin subscriptable-name-mixin)
                                       &rest initargs)
  (declare (ignore initargs))
  (setf (subscript name-mixin)
        (funcall (subscript-generator name-mixin) (name name-mixin))))

;;; This generic function appears to be nowhere used.  
(defgeneric subscripted-name (name-mixin))

(defmethod subscripted-name ((name-mixin subscriptable-name-mixin))
  ;; Perhaps this could be written as a single format statement?
  (if (/= (subscript name-mixin) 1)
      (format nil "~A <~D>" (name name-mixin) (subscript name-mixin))
      (name name-mixin)))

(defmethod (setf name) :after (new-name (name-mixin subscriptable-name-mixin))
  (setf (subscript name-mixin)
        (funcall (subscript-generator name-mixin) new-name)))

;;; "Modes" are a generally useful concept, so let's define some
;;; primitives for them here.

(defclass mode ()
  ()
  (:documentation "A superclass for all modes."))
