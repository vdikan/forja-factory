(defpackage forja-factory/siesta
  (:use
   :cl
   :cl-forja
   :cl-forja/lattices
   :cl-forja/cstructs
   :cl-forja/templates
   :cl-arrows
   :common-utils/macros)
  (:export
   :siesta-prefix
   :siesta-bin
   :pseudos-dir
   :run-dir-base
   #:siesta-command
   #:run-dir
   #:ps-file-paths
   #:siesta-direct))
(in-package :forja-factory/siesta)


(defvar siesta-prefix nil)
(defvar siesta-bin "~/bin/siesta")
(defvar pseudos-dir #P"~/pseudos")
(defvar run-dir-base #P"/tmp/run")


(defun siesta-command ()
  (if siesta-prefix
      (format nil "~a && ~a" siesta-prefix siesta-bin)
      siesta-bin))


(defun run-dir ()
  (format nil "~a-~a"
          run-dir-base
          (sxhash (get-universal-time))))


(defun ps-file-paths (base cs)
  (loop
    for kind in (cstruct-kinds cs)
    collect (format nil "~a/~a" base (chem-kind-siesta-pseudo (cdr kind)))))


(defun siesta-direct (plist &key system-label pseudos-list structure template
                              (pseudos-dir pseudos-dir)
                              (siesta-command (siesta-command))
                              (run-dir (run-dir)))
  (mk-calculation
      (append plist
              (list :system-label system-label
                    :structure structure
                    :template template
                    :pseudos-dir pseudos-dir
                    :pseudos-list pseudos-list
                    :siesta-command siesta-command
                    :run-dir run-dir))

     (aproc (format nil "mkdir -p ~a" (get-param :run-dir))
       (uiop:wait-process proc))          ; create run-dir

     (loop
       :for ps-file :in (or pseudos-list (ps-file-paths (get-param :pseudos-dir)
                                                        (get-param :structure)))
       :do (aproc
               (format nil "cp ~a ~a/"
                       ps-file (get-param :run-dir))
             (uiop:wait-process proc)))   ; copy pseudos

     (aproc (format nil "echo \"~a\" > ~a/~a.fdf"
                    (let ((populated (plist-to-template (all-params)
                                                        (get-param :template))))
                      (if structure
                          (cstruct-to-template populated (get-param :structure) :siesta)
                          populated))
                    (get-param :run-dir)
                    (get-param :system-label))) ; populate template in the run-dir

     (aproc (format nil "cd ~a && ~a < ~a.fdf | tee ~a.out"
                    (get-param :run-dir)
                    (get-param :siesta-command)
                    (get-param :system-label)
                    (get-param :system-label))
       (let ((stream (uiop:process-info-output proc)))
         (set-param :output
                    (loop
                      for line = (read-line stream nil)
                      while line
                      do (print line)
                      collect line)))
       (if (not (= 0 (uiop:wait-process proc)))
           (block error-report
             (let ((stream (uiop:process-info-output proc)))
               (set-param :error-output
                          (loop
                            for line = (read-line stream nil)
                            while line collect line)))
             (set-status "failed"))))))
