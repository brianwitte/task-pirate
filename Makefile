LISP ?= sbcl

all:
	${LISP} --eval "(push \"$(PWD)/\" asdf:*central-registry*)" \
	        --eval "(ql:quickload :asdf)" \
                --eval '(ql:quickload :task-pirate)' \
                --eval '(asdf:make :task-pirate)' \
                --eval '(quit)'
