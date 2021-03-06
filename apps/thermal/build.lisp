(ql:quickload :forja-factory)
(in-package :forja-factory/thermal)


(defvar reset-main-after-run t)


(defun results-edn (results)
  (with-open-file (out "output.edn"
                       :direction :output
                       :if-exists :supersede)
    (format out "[~%")
    (loop
      :for rec :in results
      :do
         (progn
           (format out "{")
           (loop
             :for ent :in rec
             :do (if (sb-kernel:sequencep ent)
                     (format out "[~{~f ~}]~%" (coerce ent 'list))
                     (if (keywordp ent)
                         (format out ":~a  " (string-downcase (symbol-name ent)))
                         (format out "~a  " ent))))
           (format out "}~%")))
    (format out "]")))


(defun run-main-app ()
  (format t "Starting Thermal-Transport Workflow...~%")
  (load #P"config.lisp")
  (format t "Configuration file loaded~%")
  (run-main)
  (let* ((*print-right-margin* 24)
         (results (merge-results)))
   (with-open-file (out "output.lisp"
                       :direction :output
                       :if-exists :supersede)
     (pprint-fill out results))
   (results-edn results))
  (if reset-main-after-run (reset-main))
  (format t "Thermal-Transport Workflow finished!~%"))


(sb-ext:save-lisp-and-die #P"forja-thermal.app"
                          :toplevel #'forja-factory/thermal::run-main-app
                          :executable t)
