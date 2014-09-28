(in-package :cl-user)
(defpackage cl-test-more.report
  (:use :cl)
  (:import-from :cl-test-more.variables
                :*default-test-function*)
  (:export :report
           :test-report
           :passed-test-report
           :failed-test-report
           :skipped-test-report
           :comment-report
           :composed-test-report

           :test-report-p
           :passed-report-p
           :failed-report-p
           :skipped-report-p

           :notp
           :got
           :got-form
           :expected
           :report-expected-label
           :print-error-detail

           :print-error-report
           :format-report
           :print-plan-report
           :print-finalize-report

           :*indent-level*
           :indent
           :format/indent))
(in-package :cl-test-more.report)

(defvar *report-style* :tap)

(defparameter *indent-level* 0)

(defun indent (&optional (count *indent-level*))
  (make-string (* count 4) :initial-element #\Space))

(defun format/indent (destination control-string &rest format-arguments)
  (let ((output (apply #'format nil control-string format-arguments)))
    (when (ppcre:scan "^~&" control-string)
      (fresh-line destination))
    (format destination (indent))
    (format destination
            (ppcre:regex-replace-all "\\n(?!$)"
                                     output
                                     (format nil "~%~A" (indent))))))

(defclass report ()
  ((description :type (or null string)
                :initarg :description
                :initform nil)))

(defclass comment-report (report) ())

(defclass test-report (report)
  ((print-error-detail :type boolean
                       :initarg :print-error-detail
                       :initform t)))

(defclass normal-test-report (test-report)
  ((test-function :type (or function symbol)
                  :initarg :test-function
                  :initform (error ":test-function is required"))
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

(defgeneric print-error-report (stream report style)
  (:method (stream (report report) style)
    ;; Do nothing.
    )
  (:method (stream (report test-report) (style null))
    (print-error-report stream report *report-style*)))

(defgeneric format-report (stream report style &rest args)
  (:method (stream (report report) (style null) &rest args)
    (apply #'format-report stream report *report-style* args)))

(defgeneric print-plan-report (stream num style)
  (:method (stream num (style null))
    (print-plan-report stream num *report-style*))
  (:method (stream num (style t))
    ;; Do nothing
    (fresh-line stream)))

(defgeneric print-finalize-report (stream plan reports style)
  (:method (stream plan reports (style null))
    (print-finalize-report stream plan reports *report-style*)))
