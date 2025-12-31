(cl:in-package #:esclados-utils)

(defun build-menu (command-tables &rest commands)
  (labels ((get-command-name (command)
             (or (loop for table in command-tables
                       for name = (clim:command-line-name-for-command
                                   command table :errorp nil)
                       when name return name)
                 (error 'clim:command-table-error
                  :format-string "Command ~A not found in any provided command table"
                  :format-arguments (list command))))
           (make-menu-entry (entry)
             (cond ((and (listp entry)
                         (eq (first entry) :menu))
                    (list (clim:command-table-name
                           (clim:find-command-table (second entry)))
                     :menu (second entry)))
                   ((and (listp entry)
                         (eq (first entry) :submenu))
                    (list (second entry)
                     :menu (apply #'build-menu command-tables
                                  (cddr entry))))
                   ((eq entry :divider)
                    '(nil :divider :line))
                   (t (list (get-command-name
                             (clim:command-name (listed entry)))
                       :command entry)))))
    (clim:make-command-table nil
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
                    An error of type`command-table-error' will be signaled~@
                    if a command cannot be found in any of the provided~@
                    command tables."))
