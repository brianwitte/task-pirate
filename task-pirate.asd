;;;; task-pirate.asd

(asdf:defsystem #:task-pirate
  :description "Task Pirate: A task management system in Common Lisp."
  :author "Brian"
  :license "MIT"
  :version "1.0.0"
  :serial t
  :build-operation "program-op"
  :build-pathname "task-pirate"
  :entry-point "task-pirate:main"
  :depends-on (:jonathan :local-time :split-sequence :cl-json)
  :components ((:file "libpyr")
               (:file "task")
               (:file "task-manager")
               (:file "main")))
