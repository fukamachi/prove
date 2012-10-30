
(require :cl-test-more)

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



(defpackage a-test.b
  (:use :cl
		:cl-test-more
		:a-test))

(in-package :a-test.b)
(deftest d
  (diag "hi. this is d")
  (ok t))

(deftest errors
  (ok t "should pass")
  (ok nil "should fail")
  (ok (error "this should be caught by `ok' and the test continues")
	  "this text should not be shown")
  (ng t "should fail")
  (is 1 1)
  (error "this should be caught by deftest and the test terminates")
  (pass "this should not be printed"))


(defpackage a-test.e
  (:use :cl
		:cl-test-more
		:a-test))

(in-package :a-test.e)
(deftest e
  (diag "hi. this is e")
  (ok t))

(deftest errors
  (ok t "should pass")
  (ok nil "should fail")
  (terminate "terminates test")
  (pass "this should not be printed"))

(run-test-recursively :a-test)


