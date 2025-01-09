(defpackage :task-pirate
  (:use :cl :task-manager)
  (:export :main) ; Ensure :main is exported
  )

(in-package :task-pirate)

(defun parse-args ()
  (let ((args (cdr sb-ext:*posix-argv*)))
    (cond
      ((null args) (values :list nil))
      ((string= (first args) "add")
       ;; Map the positional arguments to specific keys for `add-task`.
       (let ((name (nth 0 (rest args)))
             (description (nth 1 (rest args)))
             (priority (nth 2 (rest args)))
             (tags (nthcdr 3 (rest args)))) ; Remaining arguments are tags
         (values :add
                 (list :name name
                       :description description
                       :priority priority
                       :tags tags))))
      ((string= (first args) "modify") (values :modify (rest args)))
      ((string= (first args) "delete") (values :delete (rest args)))
      ((string= (first args) "list") (values :list (rest args)))
      (t (error "Unknown command: ~A" (first args))))))

(defun main ()
  (let ((manager (make-instance 'task-manager)))
    (load-tasks manager)
    (multiple-value-bind (command args) (parse-args)
      (case command
        (:add
         (apply #'add-task manager args))
        (:modify
         (apply #'modify-task manager args))
        (:list
         (apply #'list-tasks manager args))
        (t
         (format t "Unknown command.~%"))))))
