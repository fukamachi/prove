(in-package :cl-user)
(defpackage prove.report
  (:use :cl)
  (:export :report
           :test-report
           :normal-test-report
           :passed-test-report
           :failed-test-report
           :error-test-report
           :skipped-test-report
           :comment-report
           :composed-test-report

           :test-report-p
           :passed-report-p
           :failed-report-p
           :error-report-p
           :skipped-report-p

           :plan
           :children
           :description
           :notp
           :got
           :got-form
           :expected
           :report-expected-label
           :duration
           :slow-threshold
           :print-error-detail))
(in-package :prove.report)

(defclass report ()
  ((description :type (or null string)
                :initarg :description
                :initform nil)))

(defclass comment-report (report) ())

(defclass test-report (report)
  ((duration :initarg :duration
             :initform nil)
   (slow-threshold :initarg :slow-threshold)
   (print-error-detail :type boolean
                       :initarg :print-error-detail
                       :initform t)))

(defclass normal-test-report (test-report)
  ((test-function :type (or function symbol)
                  :initarg :test-function)
   (notp :type boolean
         :initarg :notp
         :initform nil)
   (got :initarg :got
        :initform (error ":got is required"))
   (got-form :initarg :got-form
             :initform '#:unbound)
   (expected :initarg :expected
             :initform (error ":expected is required"))
   (report-expected-label :type (or null string)
                          :initarg :report-expected-label
                          :initform nil)))

(defclass composed-test-report (test-report)
  ((plan :initarg :plan
         :initform nil)
   (children :initarg :children
             :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defclass passed-test-report (normal-test-report) ())
(defclass failed-test-report (normal-test-report) ())
(defclass error-test-report (failed-test-report) ())
(defclass skipped-test-report (normal-test-report) ())

(defun test-report-p (report)
  (typep report 'test-report))

(defun passed-report-p (report)
  (typecase report
    (skipped-test-report nil)
    (passed-test-report t)
    (composed-test-report
     (every #'passed-report-p (slot-value report 'children)))
    (otherwise nil)))

(defun failed-report-p (report)
  (typecase report
    (skipped-test-report nil)
    (failed-test-report t)
    (composed-test-report
     (some #'failed-report-p (slot-value report 'children)))
    (otherwise nil)))

(defun error-report-p (report)
  (typep report 'error-test-report))

(defun skipped-report-p (report)
  (typecase report
    (skipped-test-report t)
    (composed-test-report
     (some #'skipped-report-p (slot-value report 'children)))
    (otherwise nil)))

(defmethod print-object ((report normal-test-report) stream)
  (with-slots (got notp expected description) report
    (format stream
            "#<~A RESULT: ~S, GOT: ~S, ~:[~;NOT ~]EXPECTED: ~S~:[~;~:*, DESCRIPTION: ~A~]>"
            (type-of report)
            (passed-report-p report)
            got
            notp
            expected
            description)))
