


(defpackage a
  (:use :cl)
  (:export :c))
(in-package :a)
(defclass c () ())

(defpackage a-test
  (:use :cl
		:cl-test-more
		:a))

(in-package :a-test)
(deftest c
  (diag "hi")
  (make-instance 'c)
  (ok t))

(deftest c2
  (diag "hi2")
  (make-instance 'c)
  (ok t))

(find-tests-of-package :a-test)
(run-test-package :a-test)
