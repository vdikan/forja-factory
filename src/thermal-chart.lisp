(defpackage :thermal-chart
  (:use :cl :cl-who :ps)
  (:export #:app-launch))

(in-package :thermal-chart)

(defvar meta)


(defun thermal-chart-page (siesta-flux qe-flux &optional (width 1300) (height 850))
  "Vega-Lite container page for stocks data chart."
  (cl-who:with-html-output-to-string
      (*standard-output* nil :prologue t :indent t)
    (:html
     (:head
      (:title "Vega-Lite Chart for Thermal WF")
      (:script :src "https://cdn.jsdelivr.net/npm/vega@5.20.2")
      (:script :src "https://cdn.jsdelivr.net/npm/vega-lite@5.1.0")
      (:script :src "https://cdn.jsdelivr.net/npm/vega-embed@6.17.0"))
     (:body
      (:div :id "vis")
      (:script
       :type "text/javascript"
       (cl-who:str
        (parenscript:ps*
         `(defvar thermal-vega-spec
            (create
             $schema "https://vega.github.io/schema/vega-lite/v5.json"
             description "Vega-Lite Chart for Thermal WF"
             title  (create :text ,(format nil "Thermal Flux comparison for ~a: ~a (squares) and ~a (triangles)"
                                           (getf meta :system-label) siesta-flux qe-flux)
                            "fontSize" 24)
             width  ,width
             height ,height
             data  (create :url "http://localhost:9000/data/")

             transform
             (array
              (create :calculate ,(format nil "datum['~a']" siesta-flux) :as "flux_siesta") ;relabel siesta flux
              (create :calculate ,(format nil "datum['~a']" qe-flux) :as "flux_qe") ;relabel qe flux
              (create :calculate "['Siesta-X', 'Siesta-Y', 'Siesta-Z']" :as "comp_siesta") ;labeling components
              (create :calculate "['QE-X', 'QE-Y', 'QE-Z']" :as "comp_qe") ;labeling components
              (create :flatten (array "comp_siesta" "flux_siesta"))
              (create :flatten (array "comp_qe" "flux_qe")))

             layer
             (array
              (create
               :mark (create :type "line"
                             :point (create :filled true
                                            :shape "square"
                                            :size 60))
               :encoding (create :x (create :field "STEP"
                                            :type "quantitative"
                                            :title ,(format nil "MD step number (~a each)"
                                                            (getf meta :delta-t))
                                            :axis (create "tickCount" 6 "titleFontSize" 18 "labelFontSize" 18))
                                 :y (create :field "flux_siesta"
                                            :type "quantitative"
                                            :title "Flux component value (QE units)"
                                            :axis (create "tickCount" 6 "titleFontSize" 18 "labelFontSize" 18))
                                 :color
                                 (create :field "comp_siesta"
                                         :type "nominal"
                                         ;; composite domain properties defined in SIESTA layer
                                         :legend (create "titleFontSize" 22 "labelFontSize" 22
                                                         :title "Components")
                                         :scale (create :domain (array "Siesta-X" "Siesta-Y" "Siesta-Z"
                                                                       "QE-X" "QE-Y" "QE-Z")
                                                        :range (array "#dc143c" "#228b22" "#191970"
                                                                      "#ff8c00" "#20b2aa" "#7b68ee")))))
              (create
               :mark (create :type "line"
                             :point (create :filled true
                                            :shape "triangle"
                                            :size 80))
               :encoding (create :x (create :field "STEP"
                                            :type "quantitative"
                                            :title ,(format nil "MD step number (~a each)"
                                                            (getf meta :delta-t)))
                                 :y (create :field "flux_qe"
                                            :type "quantitative"
                                            :title "Flux component value (QE units)")
                                 :color (create :field "comp_qe" :type "nominal"))))
             config (create axis (create grid t))))
         '(vega-embed "#vis" thermal-vega-spec))))))))


(defvar *app* (make-instance 'ningle:app))

(defvar *app-handler*)


(setf (ningle:route *app* "/" )
      "Thermal transport comparison wf web plotter.
Usage: /chart/:siesta/:qe/
or /chart/:width/:height/:siesta/:qe/")


(setf (ningle:route *app* "/data/" )
      #'(lambda (params)
          (declare (ignore params))
          (setf (lack.response:response-headers ningle:*response*)
                (append (lack.response:response-headers ningle:*response*)
                        (list :content-type "application/json")))
          (uiop:read-file-string "flux.json")))


(setf (ningle:route *app* "/chart/:siesta/:qe/")
      #'(lambda (params)
          (thermal-chart-page (cdr (assoc :siesta params))
                              (cdr (assoc :qe params)))))


(setf (ningle:route *app* "/chart/:width/:height/:siesta/:qe/")
      #'(lambda (params)
          (thermal-chart-page (cdr (assoc :siesta params))
                              (cdr (assoc :qe params))
                              (cdr (assoc :width params))
                              (cdr (assoc :height params)))))


;; https://stackoverflow.com/questions/48103501/deploying-common-lisp-web-applications
(defun app-launch ()
  ;; (start-app :port 9003) ;; our start-app, for example clack:clack-up
  (setf meta (jonathan:parse (uiop:read-file-string "meta.json")))
  (setf *app-handler* (clack:clackup *app* :port 9000))
  ;; let the webserver run.
  ;; warning: hardcoded "hunchentoot".
  (handler-case (bt:join-thread (find-if (lambda (th)
                                           (search "hunchentoot" (bt:thread-name th)))
                                         (bt:all-threads)))
    ;; Catch a user's C-c
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl  ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     () (progn
          (format *error-output* "Aborting.~&")
          (clack:stop *app-handler*)
          (uiop:quit)))
    (error (c) (format t "Woops, an unknown error occured:~&~a~&" c))))
