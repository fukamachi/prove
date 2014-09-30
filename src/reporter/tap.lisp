(in-package :cl-user)
(defpackage prove.reporter.tap
  (:use :cl
        :prove.report
        :prove.reporter))
(in-package :prove.reporter.tap)

(defclass tap-reporter (reporter)
  ((indent-space :initform 4)))

(defmethod format-report (stream (reporter tap-reporter) (report comment-report) &rest args)
  (declare (ignore args))
  (format/indent reporter stream "~&# ~A~%"
                 (slot-value report 'description)))

(defmethod format-report (stream (reporter tap-reporter) (report test-report) &key count)
  (with-slots (description print-error-detail) report
    (format/indent reporter stream
                   "~&~:[not ~;~]ok~:[~;~:* ~D~]~:[~;~:* - ~A~]~%"
                   (or (passed-report-p report)
                       (skipped-report-p report))
                   count
                   description)
    (print-error-report reporter report stream)))

(defmethod format-report (stream (reporter tap-reporter) (report skipped-test-report) &key count)
  (format/indent reporter stream
                 "~&ok~:[~;~:* ~D~] - skip~:[~;~:* ~A~]~%"
                 count
                 (slot-value report 'description)))

(defmethod print-error-report ((reporter tap-reporter) (report failed-test-report) stream)
  (with-slots (got got-form expected notp report-expected-label print-error-detail) report
    (when print-error-detail
      (format/indent reporter stream
                     "~&#    got: ~S~:[~*~; => ~S~]~%#    ~:[~;not ~]expected~:[~;~:* to ~A~]: ~S~%"
                     got-form
                     (not (eq got got-form))
                     got
                     notp
                     report-expected-label
                     expected))))

(defmethod print-plan-report ((reporter tap-reporter) num stream)
  (when (numberp num)
    (format-report stream
                   reporter
                   (make-instance 'report
                                  :description (format nil "1..~A" num)))))

(defmethod print-finalize-report ((reporter tap-reporter) plan reports stream)
  (let ((failed-count (count-if #'failed-report-p reports))
        (count (count-if #'test-report-p reports)))
    (cond
      ((eq plan :unspecified)
       (format/indent reporter stream
                      "~&# Tests were run but no plan was declared.~%"))
      ((and plan
            (not (= count plan)))
       (format/indent reporter stream
                      "~&# Looks like you planned ~D test~:*~P but ran ~A.~%"
                      plan count)))
    (fresh-line stream)
    (if (< 0 failed-count)
        (format/indent reporter stream
                       "# Looks like you failed ~D test~:*~P of ~A run."
                       failed-count count)
        (format/indent reporter stream "# All ~D test~:*~P passed."
                       count))
    (terpri stream)))
