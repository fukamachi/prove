(in-package :cl-user)
(defpackage cl-test-more.output
  (:use :cl)
  (:export :*test-result-output*
           :test-result-output))
(in-package :cl-test-more.output)

(defvar *test-result-output* t)

(defun test-result-output ()
  (if (eq *test-result-output* t)
      *standard-output*
      *test-result-output*))
