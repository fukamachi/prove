(in-package :cl-user)
(defpackage cl-test-more.variables
  (:use :cl)
  (:export :*default-test-function*))
(in-package :cl-test-more.variables)

(defvar *default-test-function* #'equal)

(defvar *test-result-output* t)
