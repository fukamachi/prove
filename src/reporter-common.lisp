(in-package :cl-user)
(defpackage prove.reporter-common
  (:use :cl)
  (:export :*default-reporter*
           :print-failed-reports))
(in-package :prove.reporter-common)

;;; This file contains things that should be in prove.reporter
;;; but is also used by prove-asdf.


(defvar *default-reporter* :list)

(defgeneric print-failed-reports (reporter failed-reports stream)
  (:documentation "After all tests have run, report on any tests that failed.
FAILED-REPORTS is an alist of pathnames of failed tests and the reports for those failed tests.")
  (:method ((reporter t) failed-reports stream)
    (declare (ignore reporter failed-reports stream))
    ;; Do nothing
    ))
