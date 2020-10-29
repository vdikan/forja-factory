(defpackage forja-factory/thermal
  (:use
   :cl
   :lparallel
   :bt-semaphore
   :cl-ppcre
   :cl-arrows
   :cl-forja
   :cl-forja/lattices
   :cl-forja/cstructs
   :cl-forja/templates
   :cl-forja/siesta
   :cl-forja/qe
   :common-utils/parsers
   :common-utils/macros)
  (:import-from :alexandria :make-keyword)
  (:import-from :parse-number :parse-real-number)
  (:export
   :pseudos-dir
   :run-dir-base))
(in-package :forja-factory/thermal)


(defvar system-label "vmd")

(defvar system-name "VMD calculation")

(defvar pseudos-dir "~/pseudos"
  "Common location of the pseudos")

(defvar run-dir-base "/tmp/vmd-run")

(defvar nsteps 80
  "Number of Molecular Dynamics steps.")

(defvar densitycutoff 200
  "Density cutoff value in Ry")

(defun wfscutoff ()
  "Wavefunctions cutoff energy value in Ry"
  (/ densitycutoff 4))

(defvar init-structure (make-cstruct)
  "Initial structure. Must be SETF otherwise to run the workflow.")

(defvar md-virtual-verbose "T"
  "Signal to output verbose Virtual MD information.")

(defvar md-virtual-jhart "T"
  "Request calculation of the Hartree thermal flux component")

(defvar md-virtual-jxc "T"
  "Request calculation of the XC thermal flux component")

(defvar md-virtual-jks "T"
  "Request calculation of the Kohn thermal flux component")

(defvar md-virtual-jion "T"
  "Request calculation of the ionic thermal flux component")

(defvar md-virtual-jzero "T"
  "Request calculation of the `zero` thermal flux component")

(defvar md-length-dt 1.0
  "Length of the `BASE` MD-step in femtoseconds.")

(defvar md-length-virtual-dt 0.1
  "Length of the `VIRTUAL` MD-step in femtoseconds.
Default is 10 times smaller than the `BASE`.")

(defvar md-init-temperature 1000
  "Initial electronic temperature, K.")

(defparameter template-fdf "# Siesta VMD calculation template file

SystemName    ##:system-name:##
SystemLabel   ##:system-label:##

# VMD options

MD.Virtual           T
MD.Virtual.Verbose   ##:md-virtual-verbose:##
MD.Virtual.Jhart     ##:md-virtual-jhart:##
MD.Virtual.Jxc       ##:md-virtual-jxc:##
MD.Virtual.Jks       ##:md-virtual-jks:##
MD.Virtual.Jion      ##:md-virtual-jion:##
MD.Virtual.Jzero     ##:md-virtual-jzero:##

MD.TypeOfRun                 Verlet
MD.Initial.Time.Step         1
MD.Final.Time.Step           ##:nsteps:##
MD.Length.Time.Step          ##:md-length-dt:## fs
MD.Length.Virtual.Time.Step  ##:md-length-virtual-dt:## fs
MD.Initial.Temperature       ##:md-init-temperature:## K

# Structure specification

NumberOfAtoms   ##:number-of-atoms:##
NumberOfSpecies ##:number-of-kinds:##

%block ChemicalSpeciesLabel
##:chem-kinds:##
%endblock ChemicalSpeciesLabel

LatticeConstant ##:alat-value:## ##:alat-units:##

%block LatticeParameters
##:lattice-parameters:##
%endblock LatticeParameters

AtomicCoordinatesFormat  ScaledCartesian
%block AtomicCoordinatesAndAtomicSpecies
##:atom-list:##
%endblock AtomicCoordinatesAndAtomicSpecies

## Calculation parameters
XC.Functional	GGA
XC.Authors	PBE
SpinPolarized   .false.

MeshCutoff	##:densitycutoff:## Ry

%block MeshSizes
  50  50  50
%endblock MeshSizes

DM.MixingWeight      0.3
DM.Tolerance         1.d-5
SolutionMethod       diagon
")

(defvar siesta-prefix nil)

(defvar siesta-bin "~/bin/siesta")


(defun siesta-command ()
  (if siesta-prefix
      (format nil "~a && ~a" siesta-prefix siesta-bin)
      siesta-bin))


(defun siesta-run-dir ()
  (format nil "~a/siesta" run-dir-base))


(defun ps-file-paths (base cs)
  (loop
    for kind in (cstruct-kinds cs)
    collect (format nil "~a/~a" base (chem-kind-siesta-pseudo (cdr kind)))))


(defvar siesta-extras-plist nil)


(defun siesta-plist ()
  (append
   (list :pseudos-dir pseudos-dir
         :siesta-run-dir (siesta-run-dir)
         :siesta-command (siesta-command)
         :system-name  system-name
         :system-label system-label
         :nsteps nsteps
         :densitycutoff densitycutoff
         :init-structure init-structure
         :md-virtual-verbose md-virtual-verbose
         :md-virtual-jhart   md-virtual-jhart
         :md-virtual-jxc     md-virtual-jxc
         :md-virtual-jks     md-virtual-jks
         :md-virtual-jion    md-virtual-jion
         :md-virtual-jzero   md-virtual-jzero
         :md-length-dt       md-length-dt
         :md-length-virtual-dt md-length-virtual-dt
         :md-init-temperature  md-init-temperature
         :template-fdf template-fdf)
   siesta-extras-plist))


(defparameter template-qe
  "&energy_current
    vel_input_units='CP',
    delta_t=##:md-length-virtual-dt:##,
    file_output='current_hz',
    eta=0.100,
    n_max=5,
 /
 &CONTROL
    calculation='md',
    restart_mode='from_scratch',
    pseudo_dir='##:pseudos-dir:##',
    outdir='./save',
    prefix='a_block',
    tprnfor=.true.,
 /
 &SYSTEM
    ibrav=##:ibrav:##,
    celldm(1)=##:celldm1:##,
    nat=##:number-of-atoms:##,
    ntyp=##:number-of-kinds:##,
    ecutrho=##:densitycutoff:##,
    ecutwfc=##:wfscutoff:##,
 /
 &ELECTRONS
    conv_thr = 1.D-8,
    mixing_beta = 0.7,
 /
 &IONS
    ion_velocities = 'from_input',
 /
 ATOMIC_SPECIES
##:chem-kinds:##
 ATOMIC_POSITIONS (bohr)
##:atom-list:##

 ATOMIC_VELOCITIES
##:atom-vel-list:##

 K_POINTS (Gamma)
  ")


(defvar qe-prefix nil)

(defvar qe-bin "~/bin/all_currents.x")


(defun qe-command ()
  (if qe-prefix
      (format nil "~a && ~a" qe-prefix qe-bin)
      qe-bin))


(defun qe-run-dir (num)
  (format nil "~a/qe/step-~a" run-dir-base num))


(defvar qe-extras-plist nil)


(defun qe-plist (packed-xvs)
  (append
   packed-xvs
   (list :pseudos-dir pseudos-dir
         :qe-run-dir (qe-run-dir (getf packed-xvs :step))
         :qe-command (qe-command)
         :densitycutoff densitycutoff
         :wfscutoff  (wfscutoff)
         :structure  (make-cstruct :atoms (derive-atomlist init-structure
                                                           (getf packed-xvs :x1))
                                   :kinds (cstruct-kinds init-structure)
                                   :lattice (cstruct-lattice init-structure))
         :md-length-dt   md-length-dt
         :md-length-virtual-dt md-length-virtual-dt
         :md-init-temperature  md-init-temperature
         :template-qe template-qe )
   qe-extras-plist))


;; Parsing mechanism of coordinates and velocities for QE-snapshots.
;; While running, Siesta's calculation instance will analyze the output stream
;; of Siesta's process. The @thtr version of Siesta prints distinct lines
;; with atom coordinates before and after Virtual MD step, as well as their velocities.
;; When read in runtime, data from such line will be pushed to one of the corresponding
;; buffer lists. When filled for a corresponding step, these buffers are packed
;; as a dictionary, and a 'qe-task is submitted to satellite worker thread with this
;; dictionary as an argument.

(defparameter vs-buffer nil
  "Buffer list to store atom velocities.")

(defparameter x1-buffer nil
  "Buffer list to store coordinates in the beginning of Virtual MD step.")

(defparameter x2-buffer nil
  "Buffer list to store coordinates in the end of Virtual MD step.")


(defun reset-xvs-buffers ()
  (setf vs-buffer nil)
  (setf x1-buffer nil)
  (setf x2-buffer nil))


(defun parse-xvs-line (line)
  (register-groups-bind ((#'parse-integer step-num) xvs-code
                         (#'parse-double f1) (#'parse-double f2) (#'parse-double f3))
      ;; The following long regex recognizes and parses output lines like e.g.:
      ;; "[XVS]  step: 2 X1 3.333333 6.666666 9.999999"
      ("\\[XVS\\]\\s+step:\\s*(\\d+)\\s+(VS|X1|X2)\\s+([\\-\\.\\d]+)\\s+([\\-\\.\\d]+)\\s+([\\-\\.\\d]+)"
       line :sharedp t)
    (let ((vec (vector f1 f2 f3)))
      (if vec
          (cond
            ((string-equal xvs-code "VS")
             (progn (setf vs-buffer (append vs-buffer (list vec))) step-num))
            ((string-equal xvs-code "X1")
             (progn (setf x1-buffer (append x1-buffer (list vec))) step-num))
            ((string-equal xvs-code "X2")
             (progn (setf x2-buffer (append x2-buffer (list vec))) step-num)))))))


(defun check-xvs-buffers (max-len step-num)
  (if (and (= (length vs-buffer) max-len)
           (= (length x1-buffer) max-len)
           (= (length x2-buffer) max-len))
      (list :vs (apply #'vector vs-buffer)
            :x1 (apply #'vector x1-buffer)
            :x2 (apply #'vector x2-buffer)
            :step step-num)))


(defun scan-xvs-line (max-xvs-buffer-len line)
  (let ((step (parse-xvs-line line)))
    (if step
        (let ((packed (check-xvs-buffers max-xvs-buffer-len step)))
          (if packed (progn
                       (submit-task qe-channel #'qe-task packed)
                       (reset-xvs-buffers)
                       (format t "QE task submitted for MD step ~d~%" step)))
          step))))


(defun derive-atomlist (structure gen-coords)
  "Derives new atomlist for species in STRUCTURE mapped onto GEN-COORDS."
  (let ((offset 0)
        (atomlist nil))
    (loop
      :for kind-cons :in (cstruct-atoms structure)
      :do (block derive-atomlist
            (push (cons (car kind-cons)
                        (subseq gen-coords offset
                                (+ offset (length (cdr kind-cons)))))
                  atomlist)
            (setf offset (+ offset (length (cdr kind-cons))))))
    (reverse atomlist)))


(defun qe-atom-vel-list (vs)
  (let ((res ""))
    (dolist (l (derive-atomlist init-structure vs) res)
      (setf res
            (format nil "~a~&~a" res
                    (format nil"~{~&~{~4a ~,12f ~,12f ~,12f~}~}"
                            (map 'list (lambda (vec) (list
                                                      (string-capitalize
                                                       (symbol-name (car l)))
                                                      (aref vec 0)
                                                      (aref vec 1)
                                                      (aref vec 2)))
                                 (cdr l))))))))

;; Concurrency mechanism definitions.
;; In this workflow, a number of workers - 2 by default -  are subscribed
;; to a channel with results from the ongoing Siesta MD run
;; and dispatch spin-off QE calculations (MD snapshots).

(defun init-kernel ()
  (setf *kernel* (make-kernel 2 :name "qe-channel-kernel")))


(defun shutdown-kernel () (end-kernel :wait t))


(defparameter qe-calc-lst nil
  "This list will accumulate all QE calculations after they were run.
      It is like a session object")


(defun print-vel-dat (vs step-num)
  "An utility to make velocities record used by QE all_currents.x"
  (format nil "# Velocities at step ~d:~%~{~&~{~,12@f ~,12@f ~,12@f~}~}"
          step-num (map 'list (lambda (vec) (list (aref vec 0) (aref vec 1) (aref vec 2))) vs)))


(defun parse-qe-flux (line)
  (register-groups-bind (flux-comp (#'parse-double f1) (#'parse-double f2) (#'parse-double f3))
      ("([\\w]+)[:]*\\s*([E\\+\\-\\.\\d]+)\\s+([E\\+\\-\\.\\d]+)\\s+([E\\+\\-\\.\\d]+)"
       line :sharedp t)
    (let ((vec (vector f1 f2 f3)))
      (list (make-keyword (string-upcase (format nil "QE-~a" flux-comp))) vec))))


(defun qe-calc-result (qe-calc)
  (alexandria:flatten
   (loop for line in (funcall qe-calc :get :output)
         collect (parse-qe-flux line))))


(defun append-qe-result (qe-calc)
  (let ((result (find-if (lambda (res) (= (getf res :step)
                                          (funcall qe-calc :get :step)))
                         results-lst :from-end t)))
    (setf result (append result (qe-calc-result qe-calc)))))


(defun merge-results ()
  (loop for c in qe-calc-lst collect (append-qe-result c)))


(defparameter results-lst nil)

(defparameter siesta-result-buffer nil)


(defun parse-siesta-vmd-start (line)
  (register-groups-bind ((#'parse-integer n))
      ("\\[VMD.Logic.Start\\]\\s*([\\d]+)" line :sharedp t)
    (list :step n)))


(defun parse-siesta-vmd-stop (line)
  (register-groups-bind ((#'parse-integer n))
      ("\\[VMD.Logic.Stop\\]\\s*([\\d]+)" line :sharedp t)
    (list :step n)))


(defun parse-siesta-flux (line)
  (register-groups-bind (flux-comp (#'parse-double f1) (#'parse-double f2) (#'parse-double f3))
      ("\\[(J[\\w]+)\\]\\s*([E\\+\\-\\.\\d]+)\\s+([E\\+\\-\\.\\d]+)\\s+([E\\+\\-\\.\\d]+)"
       line :sharedp t)
    (let ((vec (vector f1 f2 f3))) (list (make-keyword (string-upcase flux-comp)) vec))))


(defun parse-siesta-ion (line)
  (register-groups-bind (ion-flux-comp (#'parse-double f1) (#'parse-double f2) (#'parse-double f3))
      ("\\[Jion\\] flux (A|B|C|D|E):\\s*([E\\+\\-\\.\\d]+)\\s+([E\\+\\-\\.\\d]+)\\s+([E\\+\\-\\.\\d]+)"
       line :sharedp t)
    (let ((vec (vector f1 f2 f3)))
      (list (make-keyword (string-upcase
                           (format nil "JION-~a" ion-flux-comp)))
            vec))))


;; Live result callbacks:
(defun siesta-result-callback (siesta-result-buffer)
  "This callback is called after each SIESTA-RESULT-BUFFER is registered."
  siesta-result-buffer)


(defun qe-calc-callback (qe-calc)
  "This callback is called after each satellite QE-CALC is processed and registered."
  qe-calc)


(defun scan-result-siesta (line)
  (if (parse-siesta-vmd-stop line)
      (progn
        (format t "Registering SIESTA result for step ~a~%"
                (getf siesta-result-buffer :step))
        (push (copy-list siesta-result-buffer) results-lst)

        ;;NOTE: redefine this callback in configuration script
        ;;      for runtime results analysis:
        (siesta-result-callback siesta-result-buffer)

        ;; reset buffer:
        (setf siesta-result-buffer nil))
      (let ((res (or (parse-siesta-vmd-start line)
                     (parse-siesta-flux line)
                     (parse-siesta-ion line))))
        (if res (setf siesta-result-buffer
                      (append siesta-result-buffer res))))))

;; Calculation factories' definition:

(defun qe-task (packed-xvs)
  "A task to be submitted for QE background worker.
   Provided with atom coordinates and velocities for a MD snapshot, it
   1. Creates QE calculation instance,
   2. Runs the calculation,
   3. Pushes the run calculation to the `qe-calc-lst` session list,
   4. Returns log message.
   Task definition is also a good place to register data,
   e.g. save calculation state to a database."
  (let ((qe-calc
          (mk-calculation (qe-plist packed-xvs)
            ;; Create run-dir:
            (aproc (format nil "mkdir -p ~a" (get-param :qe-run-dir))
              (uiop:wait-process proc))
            ;; Populate input file:
            (aproc (format nil "echo \"~a\" > ~a/input"
                           (-<> (get-param :template-qe)
                                (plist-to-template (all-params) <>)
                                (cstruct-to-template <> (get-param :structure) :qe)
                                (plist-to-template
                                 (list :atom-vel-list (qe-atom-vel-list (get-param :vs))) <>))
                           (get-param :qe-run-dir))
              (uiop:wait-process proc))
            ;; Run the QE `all_currents.x` calculation
            (aproc (format nil "cd ~a && ~a -in input > output"
                           (get-param :qe-run-dir)
                           (get-param :qe-command))
              (uiop:wait-process proc))
            ;; Register results
            (aproc (format nil "cd ~a && cat current_hz" (get-param :qe-run-dir))
              (let ((stream (uiop:process-info-output proc)))
                (set-param :output
                           (loop
                             for line = (read-line stream nil)
                             while line collect line)))

              (if (not (= 0 (uiop:wait-process proc)))
                  (block error-report
                    (let ((stream (uiop:process-info-output proc)))
                      (set-param :error-output
                                 (loop
                                   for line = (read-line stream nil)
                                   while line collect line)))
                    (set-status "failed")))))))

    (funcall qe-calc :run)      ; Run the calculation.
    (push qe-calc qe-calc-lst)  ; Push the instance we have just run to the session list
    ;;NOTE: redefine this callback in configuration script
    ;;      for runtime results analysis:
    (qe-calc-callback qe-calc)
    (format nil "Quantum Espresso VMD finished step ~d" ; Log message
            (funcall qe-calc :get :step))))


(defun bind-siesta-calc ()
  (setf
   (symbol-function 'siesta-calc)
   (mk-calculation (siesta-plist)   ; wrap around parameters list for siesta
     ;; Create run-dir for siesta calc:
     (aproc (format nil "mkdir -p ~a" (get-param :siesta-run-dir))
       (uiop:wait-process proc))
     ;; Copy pseudos to run-dir:
     (loop
       :for ps-file :in  (ps-file-paths (get-param :pseudos-dir)
                                        (get-param :init-structure))
       :do (aproc
               (format nil "cp ~a ~a/"
                       ps-file (get-param :siesta-run-dir))
             (uiop:wait-process proc)))
     ;; Populate .fdf-template file:
     (aproc (format nil "echo \"~a\" > ~a/~a.fdf"
                    (-<> (get-param :template-fdf)
                         (plist-to-template (all-params) <>)
                         (cstruct-to-template <> (get-param :init-structure) :siesta))
                    (get-param :siesta-run-dir)
                    (get-param :system-label))
       (uiop:wait-process proc))
     ;; Run the calculation:
     (aproc (format nil "cd ~a && ~a < ~a.fdf | tee ~a.out"
                    (get-param :siesta-run-dir)
                    (get-param :siesta-command)
                    (get-param :system-label)
                    (get-param :system-label))
       (let ((stream (uiop:process-info-output proc)))
         (set-param :output
                    (loop
                      for line = (read-line stream nil)
                      while line
                      do (or
                          (scan-xvs-line
                           (number-of-atoms (get-param :init-structure)) line)
                          (scan-result-siesta line))
                      collect line)))   ;FIXME
       (if (not (= 0 (uiop:wait-process proc)))
           (block error-report
             (let ((stream (uiop:process-info-output proc)))
               (set-param :error-output
                          (loop
                            for line = (read-line stream nil)
                            while line collect line)))
             (set-status "failed")))))))


(defun run-main ()
  (init-kernel)

  (defparameter qe-channel (make-channel)
    "The channel for QE tasks. They will be served in background by our
  2 background worker threads. If a calculation is submitted when all
  workers are busy, it will wait its turn; in this sense channels behave like queues.")

  (defparameter qe-logger
    (bt:make-thread
     (lambda ()
       (loop
         do (let ((res (try-receive-result qe-channel :timeout 0.200)))
              (if res (format *standard-output* "~a~%" res))))))
    "And an extra thread for a logging process. It will tell our main program that
    QE calculation is finished.")

  (bind-siesta-calc)
  (siesta-calc :run))


(defun reset-main ()
  (aproc (format nil "rm -r ~a" run-dir-base)
    (uiop:wait-process proc))
  (setf siesta-result-buffer nil)
  (setf results-lst nil)
  (setf qe-calc-lst nil)
  (reset-xvs-buffers)
  (bt:destroy-thread qe-logger)
  (shutdown-kernel))


(defvar workflow-thread nil
  "Main thread of the Thermal Transport workflow")


(defun run-main-thread ()
  (setf workflow-thread (bt:make-thread #'run-main :name "workflow-thread")))
