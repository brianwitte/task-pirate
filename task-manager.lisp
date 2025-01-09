(defpackage :task-manager
  (:use :cl :libpyr)
  (:import-from :task
                :make-task
                :get-data-fields)
  (:import-from :jonathan
                :to-json
                :parse)
  (:import-from :uiop
                :read-file-string)
  (:export :task-manager
           :add-task
           :modify-task
           :list-tasks
           :load-tasks
           :save-tasks))

(in-package :task-manager)

(defclass task-manager ()
  ((tasks :initform nil :accessor tasks)
   (data-file :initform (merge-pathnames ".task-pirate.json" (user-homedir-pathname))
              :accessor data-file)))

(defun save-tasks (manager)
  "Save tasks to a JSON file with clear delimiters."
  (with-open-file (out (data-file manager)
                       :direction :output
                       :if-exists :supersede)
    (write-string
     (jonathan:to-json
      (loop for task in (tasks manager)
            collect (get-data-fields task)))
     out)))

(defun load-tasks (manager)
  "Load tasks from a json file."
  (when (probe-file (data-file manager))
    (with-open-file (in (data-file manager))
      (let ((toml-data (jonathan:parse
                        (uiop:read-file-string in))))
        (setf (tasks manager)
              (mapcar (lambda (task-data)
                        (apply #'make-task task-data))
                      toml-data))))))

(defun add-task (manager &key name description due priority tags)
  "Add a new task to the manager."
  (push (make-task :name name
                   :description (or description "")
                   :created (get-universal-time)
                   :due due
                   :priority priority
                   :tags tags
                   :status "pending")
        (tasks manager))
  (save-tasks manager))

(defun modify-task (manager index &rest changes)
  "Modify an existing task by index."
  (let ((task (nth index (tasks manager))))
    (when task
      (dolist (change changes)
        (ecase (car change)
          (:name (setf (getf (get-data-fields task) :name) (cdr change)))
          (:description (setf (getf (get-data-fields task) :description) (cdr change)))
          (:due (setf (getf (get-data-fields task) :due) (cdr change)))
          (:priority (setf (getf (get-data-fields task) :priority) (cdr change)))
          (:tags (setf (getf (get-data-fields task) :tags) (cdr change)))))
      (save-tasks manager))))

(defun list-tasks (manager &key tags status)
  "List tasks filtered by tags and/or status."
  (dolist (task (tasks manager))
    (let* ((fields (get-data-fields task))
           (task-tags (getf fields :tags))
           (task-status (getf fields :status)))
      (when (and (or (null tags)
                     (intersection tags task-tags :test #'string=))
                 (or (null status)
                     (string= status task-status)))
        (format t "~A [~A] ~A~%  Due: ~A Priority: ~A Tags: ~{~A~^, ~}~%"
                (getf fields :name)
                task-status
                (getf fields :description)
                (or (getf fields :due) "None")
                (or (getf fields :priority) "None")
                task-tags)))))
