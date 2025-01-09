(defpackage :libpyr
  (:use :cl)
  (:export :make-data :match-data :get-data-fields))

(in-package :libpyr)

;; Create a Pyret-like "data" structure
(defun make-data (type &rest fields)
  "Create a data instance with a type and fields."
  (list :type type :fields fields))

;; Match a data structure against a set of patterns
(defun match-data (data patterns)
  "Match a data structure against patterns and execute the associated function."
  (let ((type (getf data :type)))
    (let ((fn (getf patterns type)))
      (if (and fn (functionp fn))
          (apply fn (getf data :fields))
          (error "No matching function for type ~A in patterns: ~A" type patterns)))))


;; Retrieve fields of a data structure
(defun get-data-fields (data)
  "Retrieve the fields of a data instance."
  (getf data :fields))
