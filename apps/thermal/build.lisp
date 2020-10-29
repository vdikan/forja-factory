(ql:quickload :forja-factory)
(in-package :forja-factory/thermal)


(defvar reset-main-after-run t)


(defun run-main-app ()
  (load #P"config.lisp")
  (run-main)
  (with-open-file (out "output.lisp"
                       :direction :output
                       :if-exists :supersede)
    (print (merge-results) out))
  (if reset-main-after-run (reset-main))
  (print "Thermal-Transport Workflow finished!"))


(sb-ext:save-lisp-and-die #P"forja-thermal.app"
                          :toplevel #'forja-factory/thermal::run-main-app
                          :executable t)
