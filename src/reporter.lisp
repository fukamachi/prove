(in-package :cl-user)
(defpackage prove.reporter
  (:use :cl)
  (:import-from :prove.report
                :report
                :test-report
                :description
                :format/indent)
  (:export :*report-style*
           :reporter
           :format-report
           :print-error-report
           :print-plan-report
           :print-finalize-report))
(in-package :prove.reporter)

(defvar *report-style* :list)

(defclass reporter () ())

(defun find-reporter (name)
  (make-instance
   (intern (format nil "~A-~A" name #.(string :reporter))
           (intern (format nil "~A.~A"
                           #.(string :prove.reporter)
                           name)
                   :keyword))))

(defgeneric format-report (stream reporter report &rest args)
  (:method (stream (reporter null) (report report) &rest args)
    (apply #'format-report
           stream
           (find-reporter *report-style*)
           report
           args))
  (:method (stream (reporter reporter) (report report) &rest args)
    (declare (ignore args))
    (format/indent stream "~&~A~%"
                   (slot-value report 'description))))

(defgeneric print-error-report (reporter report stream)
  (:method ((reporter reporter) (report report) stream)
    ;; Do nothing.
    )
  (:method ((reporter null) (report test-report) stream)
    (print-error-report (find-reporter *report-style*) report stream)))

(defgeneric print-plan-report (reporter num stream)
  (:method ((reporter null) num stream)
    (print-plan-report (find-reporter *report-style*) num stream))
  (:method ((reporter t) num stream)
    (declare (ignore reporter num))
    ;; Do nothing
    ))

(defgeneric print-finalize-report (reporter plan reports stream)
  (:method ((reporter null) plan reports  stream)
    (print-finalize-report (find-reporter *report-style*)
                           plan
                           reports
                           stream)))
