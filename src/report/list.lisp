(in-package :cl-user)
(defpackage cl-test-more.report.list
  (:use :cl
        :cl-test-more.report)
  (:import-from :cl-test-more.color
                :with-color))
(in-package :cl-test-more.report.list)

(defmethod format-report (stream (report report) (style (eql :list)) &rest args)
  (declare (ignore args))
  (format/indent stream "~&# ~A~2%"
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

(defun print-duration (stream duration slow-threshold)
  (let ((color (cond
                 ((< slow-threshold duration) :red)
                 ((< (/ slow-threshold 2) duration) :yellow))))
    (when color
      (with-color (color :stream stream)
        (format stream "(~Dms)" duration)))))

(defmethod format-report (stream (report normal-test-report) (style (eql :list)) &rest args)
  (declare (ignore args))
  (format/indent stream "~&  ")
  (with-color ((if (skipped-report-p report)
                   :cyan
                   :green) :stream stream)
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

(defmethod format-report (stream (report failed-test-report) (style (eql :list)) &rest args)
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

(defmethod format-report (stream (report composed-test-report) (style (eql :list)) &rest args)
  (declare (ignore args))
  (flet ((print-duration-in-gray ()
           (when (slot-value report 'duration)
             (format stream " ")
             (with-color (:gray :stream stream)
               (format stream "(~Dms)" (slot-value report 'duration))))))
    (format/indent stream "~&  ")
    (if (failed-report-p report)
        (with-color (:red :stream stream)
          (format stream "×")
          (format stream " ~:[(no description)~;~:*~A~]" (slot-value report 'description))
          (print-duration-in-gray))
        (progn
          (with-color ((if (skipped-report-p report)
                           :cyan
                           :green) :stream stream)
            (format stream "✓"))
          (format stream " ~:[(no description)~;~:*~A~]" (slot-value report 'description))
          (print-duration-in-gray))))
  (terpri stream))

(defmethod print-plan-report (stream num (style (eql :list)))
  (when num
    (format-report stream
                   (make-instance 'report
                                  :description (format nil "1..~A" num))
                   :list)))

(defmethod print-finalize-report (stream plan reports (style (eql :list)))
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
    (terpri stream)
    (unless (zerop skipped-count)
      (with-color (:cyan :stream stream)
        (format/indent stream "● ~D test~:*~P skipped" skipped-count))
      (terpri stream))))
