(in-package :cl-user)
(defpackage prove.reporter.fiveam
  (:use :cl
        :prove.report
        :prove.reporter))
(in-package :prove.reporter.fiveam)

(defclass fiveam-reporter (reporter) ())

(defmethod format-report (stream (reporter fiveam-reporter) (report comment-report) &rest args)
  (declare (ignore stream reporter report args))
  ;; Do nothing. This reporter doesn't support 'diag'.
  )

(defmethod format-report (stream (reporter fiveam-reporter) (report test-report) &rest args)
  (declare (ignore args))
  (when (zerop *indent-level*)
    (write-char (if (failed-report-p report) #\f #\.) stream)))

(defmethod print-error-report ((reporter fiveam-reporter) (report failed-test-report) stream)
  (with-slots (description got got-form expected notp report-expected-label print-error-detail) report
    (cond
      (print-error-detail
       (format/indent reporter
                      stream "~& ~:[(no description)~;~:*~A~]:~%    ~S~:[~*~; => ~S~]~%        is ~:[~;not ~]expected to ~:[be~;~:*~A~]~%    ~S~%"
                      description
                      got-form
                      (not (eq got got-form))
                      got
                      notp
                      report-expected-label
                      expected))
      (T (format/indent reporter stream "~& ~:[(no description)~;~:*~A~]: Failed~%"
                        description)))))

(defmethod print-error-report ((reporter fiveam-reporter) (report composed-test-report) stream)
  (with-slots (plan children description) report
    (format/indent reporter stream "~& ~:[(no description)~;~:*~A~]:~%"
                   description)
    (let ((*indent-level* (1+ *indent-level*)))
      (print-finalize-report reporter plan children stream))))

(defmethod print-error-report ((reporter fiveam-reporter) (report comment-report) stream)
  (format/indent reporter stream "~& ~A~%"
                 (slot-value report 'description)))

(defmethod print-finalize-report ((reporter fiveam-reporter) plan reports stream)
  (let ((failed-count (count-if #'failed-report-p reports))
        (passed-count (count-if #'passed-report-p reports))
        (skipped-count (count-if #'skipped-report-p reports))
        (count (count-if #'test-report-p reports)))
    (format/indent reporter stream
                   "~& Did ~D checks.~:[~*~; (planned ~D tests)~]~%"
                   count
                   (not (eql plan count))
                   plan)
    (unless (zerop count)
      (format/indent reporter
                     stream "    Pass: ~D (~3D%)~%" passed-count (round (* (/ passed-count count) 100)))
      (unless (zerop skipped-count)
        (format/indent reporter
                       stream "    Skip: ~D (~3D%)~%" skipped-count (round (* (/ skipped-count count) 100))))
      (format/indent reporter
                     stream "    Fail: ~D (~3D%)~%" failed-count (round (* (/ failed-count count) 100))))
    (unless (zerop failed-count)
      (format/indent reporter
                     stream "~2& Failure Details:~% --------------------------------~%")
      (loop for report across reports
            when (failed-report-p report)
              do (print-error-report reporter report stream)
                 (format/indent reporter stream " --------------------------------~%")))))
