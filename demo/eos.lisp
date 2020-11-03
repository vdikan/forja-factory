(ql:quickload :forja-factory)
(ql:quickload :cl-ppcre)

(in-package :forja-factory/siesta)

(setf system-label "eos"
      pseudos-dir "./"
      results-dir "./"
      init-pseudos-list nil
      init-structure nil
      init-template "Here should go the .fdf for ##:system-label:## calculation!")

(setf alat-list '())

(defun run-dir (alat)
  (format nil "~a/~a/~a" run-dir-base system-label alat))

(setf results '())


(defun parse-toten (line)
  (cl-ppcre:register-groups-bind ((#'clu/p:parse-double toten))
      ("siesta:\\s+Total\\s+=\\s+([\\-\\.\\d]+)" line :sharedp t) toten))


(defun run-main ()
  (loop
    :for alat :in alat-list
    :do (let ((calc                     ; creating a Forja calculation with `siesta-direct` factory..
                (siesta-direct (list :alat alat) ; for this lattice constant value
                               :system-label system-label ; and the following default parameters
                               :pseudos-list init-pseudos-list ; explicitly placed
                               :structure init-structure
                               :template init-template
                               :run-dir (run-dir alat))))
          (print (format nil "Created SIESTA calculation for alat=~a" alat))
          (funcall calc :run) ; run the calc
          (if (string-equal "finished" (funcall calc :status))
              (progn
                (print (format nil "Finished SIESTA calculation for alat=~a" alat))
                (loop
                  :for line :in (funcall calc :get :output)
                  :do (let ((toten (parse-toten line)))
                        (if toten (push (cons alat toten) results))))
                (print (format nil "Registered result for alat=~a" alat)))
              (print (format nil "calculation for alat=~a FAILED" alat)))))
  (with-open-file (out "output" :direction :output :if-exists :supersede)
    (loop :for res :in (reverse results)
          :do (format out "~a   ~a~%" (car res) (cdr res))))
  (print (format nil "Results written in output")))
