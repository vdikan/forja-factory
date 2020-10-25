(defpackage forja-factory/tests/siesta
  (:use :cl
        :forja-factory/siesta
        :rove))
(in-package :forja-factory/tests/siesta)


(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
