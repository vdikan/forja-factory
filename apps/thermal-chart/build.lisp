(ql:quickload :forja-factory)
(ql:quickload :forja-charts)
(in-package :thermal-chart)

(sb-ext:save-lisp-and-die #P"thermal-chart"
                          :toplevel #'thermal-chart::app-launch
                          :executable t)
