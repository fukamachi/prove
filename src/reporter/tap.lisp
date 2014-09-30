(in-package :cl-user)
(defpackage cl-test-more.reporter.tap
  (:use :cl
        :cl-test-more.report
        :cl-test-more.reporter)
  (:import-from :cl-test-more.color
                :with-color))
(in-package :cl-test-more.reporter.tap)

(defclass tap-reporter (reporter) ())

(defmethod format-report (stream (reporter tap-reporter) (report report) &rest args)
  (declare (ignore args))
  (format/indent stream "~&~A~%"
                 (slot-value report 'description)))

(defmethod format-report (stream (reporter tap-reporter) (report comment-report) &rest args)
  (declare (ignore args))
  (format/indent stream "~&# ~A~%"
                 (slot-value report 'description)))

(defmethod format-report (stream (reporter tap-reporter) (report test-report) &key count)
  (with-slots (description print-error-detail) report
    (format/indent stream
                   "~&~:[not ~;~]ok~:[~;~:* ~D~]~:[~;~:* - ~A~]~%"
                   (or (passed-report-p report)
                       (skipped-report-p report))
                   count
                   description)
    (when (and (failed-report-p report)
               print-error-detail)
      (print-error-report reporter report stream))))

(defmethod format-report (stream (reporter tap-reporter) (report skipped-test-report) &key count)
  (with-slots (description print-error-detail) report
    (format/indent stream
                   "~&ok~:[~;~:* ~D~] - skip~:[~;~:* ~A~]~%"
                   count
                   description)))

(defmethod print-error-report ((reporter tap-reporter) (report normal-test-report) stream)
  (with-slots (got got-form expected notp report-expected-label) report
    (when (failed-report-p report)
      (format/indent stream
                     "~&#    got: ~S~:[~*~; => ~S~]~%#    ~:[~;not ~]expected~:[~;~:* to ~A~]: ~S~%"
                     got-form
                     (not (eq got got-form))
                     got
                     notp
                     report-expected-label
                     expected))))

(defmethod print-plan-report ((reporter tap-reporter) num stream)
  (when num
    (format-report stream
                   reporter
                   (make-instance 'report
                                  :description (format nil "1..~A" num)))))

(defmethod print-finalize-report ((reporter tap-reporter) plan reports stream)
  (let ((failed-count (count-if #'failed-report-p reports))
        (count (count-if #'test-report-p reports)))
    (cond
      ((eq plan :unspecified)
       (format/indent stream
                      "~&# Tests were run but no plan was declared.~%"))
      ((and plan
            (not (= count plan)))
       (format/indent stream
                      "~&# Looks like you planned ~D test~:*~P but ran ~A.~%"
                      plan count)))
    (fresh-line stream)
    (if (< 0 failed-count)
        (with-color (:red :stream stream)
          (format/indent stream
                         "# Looks like you failed ~D test~:*~P of ~A run."
                         failed-count count))
        (with-color (:green :stream stream)
          (format/indent stream "# All ~D test~:*~P passed."
                         count)))
    (terpri stream)))
