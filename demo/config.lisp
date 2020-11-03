(in-package :forja-factory/siesta)  ; need to select proper namespace/package

(setf init-template (uiop:read-file-string "Si.fdf")) ; read a template fdf-file

(setf init-pseudos-list '("Si.psf")) ; specify pseudos file

(setf alat-list '(5.35 5.37 5.39 5.41 5.43 5.45 5.47 5.49)) ; define the latttice constants range

;; Concretize other defaults if required, e.g.:
;; (setf run-dir-base "/tmp/run")
;; (setf siesta-bin  "~/bin/siesta")
;; (setf siesta-prefix nil)
;; ...
