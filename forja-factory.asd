(defsystem "forja-factory"
  :version "0.1.0"
  :author "Vladimir Dikan"
  :license "GPL"
  :depends-on ("lparallel"
               "bt-semaphore"
               "cl-arrows"
               "cl-forja"
               "common-utils")
  :components ((:module "src"
                :components
                ((:file "siesta")
                 (:file "thermal"))))
  :description "Some factory functions for CL-Forja calculations."
  :in-order-to ((test-op (test-op "forja-factory/tests"))))


(defsystem "forja-charts"
  :version "0.1.0"
  :author "Vladimir Dikan"
  :license "GPL"
  :depends-on ("cl-who"
               "parenscript"
               "clack"
               "ningle")
  :components ((:module "src"
                :components
                ((:file "thermal-chart"))))
  :description "Auxiliary web-app charts with Vega Lite.")


(defsystem "forja-factory/tests"
  :author "Vladimir Dikan"
  :license "GPL"
  :depends-on ("forja-factory"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "siesta"))))
  :description "Test system for forja-factory"
  :perform (test-op (op c) (symbol-call :rove :run c)))

;; NOTE: To run this test file, execute `(asdf:test-system :forja-factory)' in your Lisp.
