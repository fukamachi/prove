(in-package :cl-user)
(defpackage prove.reporter
  (:use :cl)
  (:import-from :prove.report
                :report
                :test-report
                :description)
  (:import-from :prove.reporter-common
                :*default-reporter*
                :print-failed-reports)
  (:export :*indent-level*
           :*default-reporter*
           :indent-space
           :format/indent

           :reporter
           :format-report
           :print-error-report
           :print-plan-report
           :print-finalize-report
           :print-failed-reports))
(in-package :prove.reporter)

(defparameter *indent-level* 0)

(defun indent (space &optional (count *indent-level*))
  (make-string (* count space) :initial-element #\Space))

(defun format/indent (reporter destination control-string &rest format-arguments)
  (let ((output (apply #'format nil control-string format-arguments)))
    (when (ppcre:scan "^~&" control-string)
      (fresh-line destination))
    (with-slots (indent-space) reporter
      (format destination (indent indent-space))
      (write-string (ppcre:regex-replace-all "(\\n)(?!$)"
                                             output
                                             (format nil "\\1~A" (indent indent-space)))
                    destination))))

(defclass reporter ()
  ((indent-space :initform 2)))

(defun find-reporter (name)
  (make-instance
   (intern (format nil "~:@(~A~)-~A" name #.(string :reporter))
           (intern (format nil "~A.~:@(~A~)"
                           #.(string :prove.reporter)
                           name)
                   :keyword))))

(defgeneric format-report (stream reporter report &rest args)
  (:method (stream (reporter null) (report report) &rest args)
    (apply #'format-report
           stream
           (find-reporter *default-reporter*)
           report
           args))
  (:method (stream (reporter reporter) (report report) &rest args)
    (declare (ignore args))
    (format/indent reporter stream "~&~A~%"
                   (slot-value report 'description))))

(defgeneric print-error-report (reporter report stream)
  (:method ((reporter reporter) (report report) stream)
    ;; Do nothing.
    )
  (:method ((reporter null) (report test-report) stream)
    (print-error-report (find-reporter *default-reporter*) report stream)))

(defgeneric print-plan-report (reporter num stream)
  (:method ((reporter null) num stream)
    (print-plan-report (find-reporter *default-reporter*) num stream))
  (:method ((reporter t) num stream)
    (declare (ignore reporter num))
    ;; Do nothing
    ))

(defgeneric print-finalize-report (reporter plan reports stream)
  (:method ((reporter null) plan reports  stream)
    (print-finalize-report (find-reporter *default-reporter*)
                           plan
                           reports
                           stream)))

(defmethod print-failed-reports ((reporter null) failed-reports stream)
  (print-failed-reports (find-reporter *default-reporter*)
                        failed-reports
                        stream))
(defmethod print-failed-reports ((reporter reporter) failed-reports stream)
  (declare (list failed-reports))
  (format/indent reporter stream "~2&Failed Tests:~%")
  (loop for (path . reports) in failed-reports do
       (progn
         (format/indent reporter stream "~a:" path)
         (let ((*indent-level* (1+ *indent-level*)))
           (dolist (report reports)
             (format-report stream reporter report))))))
