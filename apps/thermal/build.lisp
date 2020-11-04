(ql:quickload :forja-factory)
(in-package :forja-factory/thermal)


(defvar reset-main-after-run t)


(defun run-main-app ()
  (format t "Starting Thermal-Transport Workflow...~%")
  (load #P"config.lisp")
  (format t "Configuration file loaded~%")
  (run-main)
  (let ((*print-right-margin* 24))
   (with-open-file (out "output.lisp"
                       :direction :output
                       :if-exists :supersede)
    (pprint-fill out (merge-results))))
  (if reset-main-after-run (reset-main))
  (format t "Thermal-Transport Workflow finished!~%"))


(sb-ext:save-lisp-and-die #P"forja-thermal.app"
                          :toplevel #'forja-factory/thermal::run-main-app
                          :executable t)
