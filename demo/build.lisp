(ql:quickload :forja-factory)
(load #P"eos.lisp")

(in-package :forja-factory/siesta)


(defvar cleanup-after-run t)


(defun run-main-app ()
  (load #P"config.lisp")
  (print "Starting EOS workflow...")
  (run-main)
  (if cleanup-after-run (uiop:run-program (format nil "rm -r ~a" run-dir-base)))
  (print "EOS Workflow finished!"))


(sb-ext:save-lisp-and-die #P"forja-demo.app"
                          :toplevel #'forja-factory/siesta::run-main-app
                          :executable t)
