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
  (format/indent stream "~&  ~A~%"
                 (slot-value report 'description)))

(defun possible-report-description (report)
  (cond
    ((slot-value report 'description)
     (format nil "~A~:[~; (Skipped)~]"
             (slot-value report 'description)
             (skipped-report-p report)))
    ((and (typep report 'normal-test-report)
          (slot-value report 'got-form))
     (with-slots (got got-form notp report-expected-label expected) report
       (format nil "~S is ~:[~;not ~]expected to ~:[be~;~:*~A~] ~S~:[ (got ~S)~;~*~]"
               got-form
               notp
               report-expected-label
               expected
               (eq got got-form)
               got)))))

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
  (format/indent stream "~&  ")
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
  (format/indent stream "~&  ")
  (with-color (:cyan :stream stream)
    (format stream "-")
    (let ((description (possible-report-description report)))
      (when description
        (format stream " ")
        (write-string description stream))))
  (terpri stream))

(defmethod format-report (stream (reporter list-reporter) (report failed-test-report) &rest args)
  (declare (ignore args))
  (format/indent stream "~&  ")
  (with-color (:red :stream stream)
    (format stream "×")
    (let ((description (possible-report-description report))
          (duration (slot-value report 'duration)))
      (when description
        (format stream " ")
        (write-string description stream))
      (when duration
        (format stream " ")
        (print-duration stream duration (slot-value report 'slow-threshold)))))
  (terpri stream))

(defmethod format-report (stream (reporter list-reporter) (report composed-test-report) &rest args)
  (declare (ignore args))
  ;; Do nothing
  )

(defmethod print-plan-report ((reporter list-reporter) num stream)
  (when (numberp num)
    (format/indent stream "~&1..~A~2%" num)))

(defmethod print-finalize-report ((reporter list-reporter) plan reports stream)
  (let ((failed-count (count-if #'failed-report-p reports))
        (skipped-count (count-if #'skipped-report-p reports))
        (count (count-if #'test-report-p reports)))
    (format/indent stream "~2&")
    (cond
      ((eq plan :unspecified)
       (with-color (:yellow :stream stream)
         (format/indent stream
                        "△ Tests were run but no plan was declared.~%")))
      ((and plan
            (not (= count plan)))
       (with-color (:yellow :stream stream)
         (format/indent stream
                        "△ Looks like you planned ~D test~:*~P but ran ~A.~%"
                        plan count))))
    (if (< 0 failed-count)
        (with-color (:red :stream stream)
          (format/indent stream
                         "× ~D of ~D test~:*~P failed"
                         failed-count count))
        (with-color (:green :stream stream)
          (format/indent stream
                         "✓ ~D test~:*~P completed" count)))
    (format stream " ")
    (print-duration stream
                    (reduce #'+
                            (remove-if-not #'test-report-p reports)
                            :key (lambda (report) (or (slot-value report 'duration) 0))))
    (terpri stream)
    (unless (zerop skipped-count)
      (with-color (:cyan :stream stream)
        (format/indent stream "● ~D test~:*~P skipped" skipped-count))
      (terpri stream))))
