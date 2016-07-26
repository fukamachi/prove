(in-package :cl-user)
(defpackage prove.output
  (:use :cl)
  (:export :*test-result-output*
           :test-result-output))
(in-package :prove.output)

(defvar *test-result-output* t)

(defun test-result-output ()
  (if (eq *test-result-output* t)
      *standard-output*
      *test-result-output*))
