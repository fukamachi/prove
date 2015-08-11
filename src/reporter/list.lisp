(in-package :cl-user)
(defpackage prove.reporter.list
  (:use :cl
        :prove.report
        :prove.reporter)
  (:import-from :prove.color
                :with-color)
  (:export :list-reporter))
(in-package :prove.reporter.list)

(defclass list-reporter (reporter) ())

(defmethod format-report (stream (reporter list-reporter) (report comment-report) &rest args)
  (declare (ignore args))
  (format/indent reporter stream "~& ")
  (with-color (:white :stream stream)
    (princ (slot-value report 'description) stream))
  (terpri stream))

(defun omit-long-value (value)
  (typecase value
    (string
     (if (< 500 (length value))
         (format nil "\"~A ...\"" (subseq value 0 94))
         (prin1-to-string value)))
    (otherwise
     (let ((value (prin1-to-string value)))
       (if (< 500 (length value))
           (format nil "~A ..." (subseq value 0 96))
           value)))))

(defun report-expected-line (report)
  (when (typep report 'normal-test-report)
    (with-slots (got got-form notp report-expected-label expected) report
      (format nil "~A is ~:[~;not ~]expected to ~:[be~;~:*~A~] ~A~:[ (got ~S)~;~*~]"
              (omit-long-value (or got-form got))
              notp
              report-expected-label
              (omit-long-value expected)
              (eq got got-form)
              got))))

(defun possible-report-description (report)
  (cond
    ((slot-value report 'description)
     (format nil "~A~:[~; (Skipped)~]"
             (slot-value report 'description)
             (skipped-report-p report)))
    (T (report-expected-line report))))

(defun print-duration (stream duration &optional slow-threshold)
  (let ((color (if slow-threshold
                   (cond
                     ((< slow-threshold duration) :red)
                     ((< (/ slow-threshold 2) duration) :yellow))
                   :gray)))
    (when color
      (with-color (color :stream stream)
        (format stream "(~Dms)" duration)))))

(defmethod format-report (stream (reporter list-reporter) (report normal-test-report) &rest args)
  (declare (ignore args))
  (format/indent reporter stream "~&  ")
  (with-color (:green :stream stream)
    (format stream "✓"))
  (let ((description (possible-report-description report))
        (duration (slot-value report 'duration)))
    (when description
      (format stream " ")
      (with-color (:gray :stream stream)
        (write-string description stream)))
    (when duration
      (format stream " ")
      (print-duration stream duration (slot-value report 'slow-threshold))))
  (terpri stream))

(defmethod format-report (stream (reporter list-reporter) (report skipped-test-report) &rest args)
  (declare (ignore args))
  (format/indent reporter stream "~&  ")
  (with-color (:cyan :stream stream)
    (format stream "-")
    (let ((description (possible-report-description report)))
      (when description
        (format stream " ")
        (write-string description stream))))
  (terpri stream))

(defmethod format-report (stream (reporter list-reporter) (report failed-test-report) &rest args)
  (declare (ignore args))
  (format/indent reporter stream "~&  ")
  (with-color (:red :stream stream)
    (format stream "×")
    (let ((description (possible-report-description report))
          (duration (slot-value report 'duration)))
      (when description
        (format stream " ")
        (write-string description stream))
      (when duration
        (format stream " ")
        (print-duration stream duration (slot-value report 'slow-threshold))))
    (when (slot-value report 'description)
      (format/indent reporter stream "~&    ")
      (princ (report-expected-line report) stream)))
  (terpri stream))

(defmethod format-report (stream (reporter list-reporter) (report error-test-report) &rest args)
  (declare (ignore args))
  (format/indent reporter stream "~&  ")
  (with-color (:red :stream stream)
    (format stream "× ")
    (when (slot-value report 'description)
      (format stream "~A~%" (slot-value report 'description))
      (format/indent reporter stream "    "))
    (format stream "Raised an error ~A (expected: ~S)"
            (slot-value report 'got)
            (slot-value report 'expected)))
  (terpri stream))

(defmethod format-report (stream (reporter list-reporter) (report composed-test-report) &rest args)
  (declare (ignore args))
  ;; Do nothing
  )

(defmethod print-plan-report ((reporter list-reporter) num stream)
  (when (numberp num)
    (format/indent reporter stream "~&1..~A~2%" num)))

(defmethod print-finalize-report ((reporter list-reporter) plan reports stream)
  (let ((failed-count (count-if #'failed-report-p reports))
        (skipped-count (count-if #'skipped-report-p reports))
        (count (count-if #'test-report-p reports)))
    (format/indent reporter stream "~2&")
    (cond
      ((eq plan :unspecified)
       (with-color (:yellow :stream stream)
         (format/indent reporter stream
                        "△ Tests were run but no plan was declared.~%")))
      ((and plan
            (not (= count plan)))
       (with-color (:yellow :stream stream)
         (format/indent reporter stream
                        "△ Looks like you planned ~D test~:*~P but ran ~A.~%"
                        plan count))))
    (if (< 0 failed-count)
        (with-color (:red :stream stream)
          (format/indent reporter stream
                         "× ~D of ~D test~:*~P failed"
                         failed-count count))
        (with-color (:green :stream stream)
          (format/indent reporter stream
                         "✓ ~D test~:*~P completed" count)))
    (format stream " ")
    (print-duration stream
                    (reduce #'+
                            (remove-if-not #'test-report-p reports)
                            :key (lambda (report) (or (slot-value report 'duration) 0))))
    (terpri stream)
    (unless (zerop skipped-count)
      (with-color (:cyan :stream stream)
        (format/indent reporter stream "● ~D test~:*~P skipped" skipped-count))
      (terpri stream))))
