(in-package :cl-user)
(defpackage prove.output
  (:use :cl)
  (:export :*test-result-output*
           :test-result-output
           :*default-reporter*))
(in-package :prove.output)

(defvar *test-result-output* t)

(defun test-result-output ()
  (if (eq *test-result-output* t)
      *standard-output*
      *test-result-output*))

;; This should be in prove.reporter,
;; but it's here because this will also be used in prove-asdf.
(defvar *default-reporter* :list)
