(in-package :cl-user)
(defpackage cl-test-more.suite
  (:use :cl)
  (:import-from :cl-test-more.output
                :test-result-output)
  (:import-from :cl-test-more.report
                :report
                :print-plan-report
                :print-finalize-report)
  (:export :*suite*
           :current-suite

           :suite
           :package-suite
           :suite-plan
           :test-count
           :failed
           :reports

           :add-report
           :plan
           :finalize))
(in-package :cl-test-more.suite)

(defparameter *suite* nil)

(defclass suite ()
  ((plan :initarg :plan
         :initform :unspecified
         :accessor suite-plan)
   (test-count :initform 0
               :accessor test-count)
   (failed :initform 0
           :accessor failed)
   (reports :initform (make-array 0 :adjustable t :fill-pointer 0)
            :accessor reports)))

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
  (vector-push-extend report (slot-value suite 'reports)))

(defun plan (num)
  (let ((suite (current-suite)))
    (setf (slot-value suite 'plan) num)
    (reset-suite suite))
  (print-plan-report (test-result-output)
                     num nil))

(defun finalize (&optional (suite (current-suite)))
  (with-slots (plan reports failed) suite
    (print-finalize-report (test-result-output)
                           plan reports nil)
    (prog1
        (zerop failed)
      (reset-suite suite))))
