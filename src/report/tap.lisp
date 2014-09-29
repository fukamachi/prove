(in-package :cl-user)
(defpackage cl-test-more.report.tap
  (:use :cl)
  (:import-from :cl-test-more.report
                :report
                :test-report
                :normal-test-report
                :skipped-test-report
                :comment-report
                :passed-report-p
                :failed-report-p
                :skipped-report-p
                :test-report-p
                :got
                :got-form
                :expected
                :notp
                :report-expected-label
                :print-error-detail
                :description
                :format/indent
                :format-report
                :print-error-report
                :print-plan-report
                :print-finalize-report)
  (:import-from :cl-test-more.color
                :with-color-if-available)
  (:import-from :cl-colors
                :+white+
                :+black+
                :+green+
                :+red+))
(in-package :cl-test-more.report.tap)

(defmethod format-report (stream (report report) (style (eql :tap)) &rest args)
  (declare (ignore args))
  (format/indent stream "~&~A~%"
                 (slot-value report 'description)))

(defmethod format-report (stream (report comment-report) (style (eql :tap)) &rest args)
  (declare (ignore args))
  (format/indent stream "~&# ~A~%"
                 (slot-value report 'description)))

(defmethod format-report (stream (report test-report) (style (eql :tap)) &key count)
  (with-slots (description print-error-detail) report
    (format/indent stream
                   "~&~:[not ~;~]ok~:[~;~:* ~D~]~:[~;~:* - ~A~]~%"
                   (or (passed-report-p report)
                       (skipped-report-p report))
                   count
                   description)
    (when (and (failed-report-p report)
               print-error-detail)
      (print-error-report stream report style))))

(defmethod format-report (stream (report skipped-test-report) (style (eql :tap)) &key count)
  (with-slots (description print-error-detail) report
    (format/indent stream
                   "~&ok~:[~;~:* ~D~] - skip~:[~;~:* ~A~]~%"
                   count
                   description)))

(defmethod print-error-report (stream (report normal-test-report) (style (eql :tap)))
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

(defmethod print-plan-report (stream num (style (eql :tap)))
  (when num
    (format-report stream
                   (make-instance 'report
                                  :description (format nil "1..~A" num))
                   :tap)))

(defmethod print-finalize-report (stream plan reports (style (eql :tap)))
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
        (with-color-if-available (cl-colors:+white+ :stream stream)
          (with-color-if-available (cl-colors:+red+ :style :background :stream stream)
            (format/indent stream
                           "# Looks like you failed ~D test~:*~P of ~A run."
                           failed-count count)))
        (with-color-if-available (cl-colors:+black+ :stream stream)
          (with-color-if-available (cl-colors:+green+ :style :background :stream stream)
            (format/indent stream "# All ~D test~:*~P passed."
                           count))))
    (terpri stream)))
