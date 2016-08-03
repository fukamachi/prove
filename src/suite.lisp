(in-package :cl-user)
(defpackage prove.suite
  (:use :cl)
  (:import-from :prove.output
                :*test-result-output*)
  (:import-from :prove.report
                :report
                :failed-report-p)
  (:import-from :prove.reporter
                :print-plan-report
                :print-finalize-report)
  (:import-from :prove.asdf
                :*last-suite-report*)
  (:export :*suite*

           :current-suite

           :suite
           :package-suite
           :suite-plan
           :test-count
           :failed
           :reports
           :slow-threshold
           :*default-slow-threshold*

           :add-report
           :plan
           :finalize))
(in-package :prove.suite)

(defparameter *suite* nil)
(defparameter *default-slow-threshold* 75)

(defclass suite ()
  ((plan :initarg :plan
         :initform :unspecified
         :accessor suite-plan)
   (slow-threshold :initarg :slow-threshold
                   :initform *default-slow-threshold*)
   (test-count :initform 0
               :accessor test-count)
   (failed :initform 0
           :accessor failed)
   (reports :initform (make-array 0 :adjustable t :fill-pointer 0)
            :accessor reports)))

(defun slow-threshold (&optional new-threshold)
  (if new-threshold
      (setf (slot-value (current-suite) 'slow-threshold) new-threshold)
      (slot-value (current-suite) 'slow-threshold)))

(defclass package-suite (suite) ())

(defvar *defined-suites* (make-hash-table :test 'equal))

(defun find-package-suite (package-designator)
  (let ((package (typecase package-designator
                   (package package-designator)
                   (T (find-package package-designator)))))
    (or (gethash (package-name package) *defined-suites*)
        (setf (gethash (package-name package) *defined-suites*)
              (make-instance 'package-suite)))))

(defun current-suite ()
  (or *suite*
      (find-package-suite *package*)))

(defun reset-suite (suite)
  (with-slots (test-count failed reports) suite
    (setf test-count 0)
    (setf failed 0)
    (setf reports (make-array 0 :adjustable t :fill-pointer 0))))

(defun add-report (report suite)
  (check-type report report)
  (when (failed-report-p report)
    (incf (slot-value suite 'failed)))
  (vector-push-extend report (slot-value suite 'reports)))

(defun plan (num)
  (let ((suite (current-suite)))
    (setf (slot-value suite 'plan) num)
    (reset-suite suite))
  (print-plan-report nil num *test-result-output*))

(defun finalize (&optional (suite (current-suite)))
  (with-slots (plan reports failed) suite
    (print-finalize-report nil plan reports *test-result-output*)
    (setf *last-suite-report*
          (list :plan plan :failed failed))
    (zerop failed)))
