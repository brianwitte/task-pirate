(defpackage :task
  (:use :cl :libpyr)
  (:export :make-task :match-task :get-task-fields))

(in-package :task)

(defun make-task (&key name description created completed due priority tags status)
  "Create a task data structure."
  (make-data :task
             :name name
             :description description
             :created created
             :completed completed
             :due due
             :priority priority
             :tags tags
             :status status))

(defun match-task (task patterns)
  "Match a task data structure against patterns."
  (match-data task patterns))

(defun get-task-fields (task)
  "Retrieve fields from a task data structure."
  (get-data-fields task))
