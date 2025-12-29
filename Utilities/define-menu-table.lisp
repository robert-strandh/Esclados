(cl:in-package #:esclados-utils)

(defmacro define-menu-table (name (&rest command-tables) &body commands)
  `(clim:make-command-table ',name
    :inherit-from (list (build-menu ',command-tables ,@commands))
    :inherit-menu t
    :errorp nil))

(setf (documentation 'define-menu-table 'function)
      (format nil "Define a command table with a menu named `name'~@
                   and containing `commands'. `Command-tables' must~@
                   be a list of command table designators containing~@
                   the named commands that will be included in the menu.~@
                   `Commands' must have the same format as the `commands'~@
                    argument to `build-menu'. If `name' already names a~@
                    command table, the old definition will be destroyed."))
