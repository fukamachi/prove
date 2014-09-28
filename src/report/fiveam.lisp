(in-package :cl-user)
(defpackage cl-test-more.report.fiveam
  (:use :cl)
  (:import-from :cl-test-more.report
                :*indent-level*
                :format/indent
                :format-report
                :print-error-report
                :print-finalize-report
                :test-report-p
                :passed-report-p
                :failed-report-p
                :skipped-report-p
                :description
                :got
                :got-form
                :expected
                :notp
                :report-expected-label
                :report
                :print-error-detail
                :plan
                :children
                :comment-report
                :test-report
                :composed-test-report))
(in-package :cl-test-more.report.fiveam)

(defmethod format-report (stream (report report) (style (eql :fiveam)) &rest args)
  (declare (ignore args))
  ;; Do nothing. This reporter doesn't support 'diag'.
  )

(defmethod format-report (stream (report test-report) (style (eql :fiveam)) &rest args)
  (declare (ignore args))
  (write-char (if (failed-report-p report) #\f #\.) stream))

(defmethod print-error-report (stream (report test-report) (style (eql :fiveam)))
  (with-slots (description got got-form expected notp report-expected-label print-error-detail) report
    (cond
      ((passed-report-p report))
      (print-error-detail
       (format/indent stream "~& ~:[(no description)~;~:*~A~]:~%    ~S~:[~*~; => ~S~]~%        is ~:[~;not ~]expected to ~:[be~;~:*~A~]~%    ~S~%"
                      description
                      got-form
                      (not (eq got got-form))
                      got
                      notp
                      report-expected-label
                      expected))
      (T (format/indent stream "~& ~:[(no description)~;~:*~A~]: Failed~%"
                        description)))))

(defmethod print-error-report (stream (report composed-test-report) (style (eql :fiveam)))
  (with-slots (plan children description) report
    (format/indent stream "~& ~:[(no description)~;~:*~A~]:~%"
                   description)
    (let ((*indent-level* (1+ *indent-level*)))
      (print-finalize-report stream plan children :fiveam))))

(defmethod print-error-report (stream (report comment-report) (style (eql :fiveam)))
  (format/indent stream "~& ~A~%"
                 (slot-value report 'description)))

(defmethod print-finalize-report (stream plan reports (style (eql :fiveam)))
  (let ((failed-count (count-if #'failed-report-p reports))
        (passed-count (count-if #'passed-report-p reports))
        (skipped-count (count-if #'skipped-report-p reports))
        (count (count-if #'test-report-p reports)))
    (format/indent stream
                   "~& Did ~D checks.~:[~*~; (planned ~D tests)~]~%"
                   count
                   (not (eql plan count))
                   plan)
    (unless (zerop count)
      (format/indent stream "    Pass: ~D (~3D%)~%" passed-count (round (* (/ passed-count count) 100)))
      (unless (zerop skipped-count)
        (format/indent stream "    Skip: ~D (~3D%)~%" skipped-count (round (* (/ skipped-count count) 100))))
      (format/indent stream "    Fail: ~D (~3D%)~%" failed-count (round (* (/ failed-count count) 100))))
    (unless (zerop failed-count)
      (format/indent stream "~2& Failure Details:~% --------------------------------~%")
      (loop for report across reports
            when (failed-report-p report)
              do (print-error-report stream report :fiveam)
                 (format/indent stream " --------------------------------~%")))))
