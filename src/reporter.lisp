(in-package :cl-user)
(defpackage prove.reporter
  (:use :cl)
  (:import-from :prove.report
                :report
                :test-report
                :description)
  (:import-from :prove.output
                :*default-reporter*)
  (:export :*indent-level*
           :indent-space
           :format/indent
           :reporter
           :format-report
           :print-error-report
           :print-plan-report
           :print-finalize-report
           :with-additional-indent))
(in-package :prove.reporter)

(defparameter *indent-level* 0
  "Level for nested test-cases output.
   Number of spaces, added for each indentation level
   is described in reporter's indent-space slot.

   Also, macro shift-indent could be used to slightly
   indent content inside the main indentation level.

   full-indent = indent-space * indent-level + additional-indent

   Here is an example of the output:

   1|  x Blah minor.
   2|    Next line of description:
   3|
   4|      x Nested test.
   5|        Also has multiline description.

   In this example, indent-space is 4, that is why
   text on lines 1 and 4 have 4 spaces between the 'x'
   horizontally.

   Outputting the first line \"  x \", reporter sets
   *additional-indent* to 4. That is why these additional
   4 lines are prepended to the rest lines of the main
   test case description.

   When inner testcase runs, it increments *indent-level*,
   which shifts output to another 4 spaces (indent-space)
   to the right, simultaneously resetting *additional-indent*
   to zero.

   For nested test, reporter writes \"  x \" and again,
   sets *additional-indent* to 4 and every other lines now
   shifted by 1 * 4 + 4 = 8 spaces.
")

(defparameter *additional-indent* 0
  "Number of spaces to add to each line. see *indent-level* docstring for full description.")

(defvar *debug-indentation* nil
  "If True, then indentation will have '=' and '-' symbols for main indentaion and additional, instead of spaces.")

(defun indent (space &optional (count *indent-level*))
  "Creates a string with a number of spaces to indent new line
of a test report."
  (if *debug-indentation*
      (concatenate 'string
                   (make-string (* count space)
                                :initial-element #\=)
                   (make-string *additional-indent*
                                :initial-element #\-))
      (make-string (+ (* count space)
                                     *additional-indent*)
                   :initial-element #\space)))


(defmacro with-additional-indent ((reporter stream control-string &rest format-arguments) &body body)
  (declare (ignorable reporter stream control-string))
  (let* ((need-new-line (ppcre:scan "^~&" control-string))
         (string (apply #'format nil control-string format-arguments))
         (increment (length string)))
    `(with-slots (indent-space) reporter
       (let* ((first-line-indent (indent indent-space))
              (*additional-indent* ,(if need-new-line
                                        increment
                                        `(+ *additional-indent*
                                            ,increment))))
         (declare (ignorable first-line-indent))
         ,(if need-new-line
              `(progn (fresh-line stream)
                      (write-string first-line-indent ,stream)
                      ;; because we just started a new line, we
                      ;; should use format/indent to write string
                      ;; taking into account a main indentation level
                      (format/indent ,reporter ,stream ,string))
              ;; otherwise, just output our prefix
              `(write-string ,string ,stream))
       
       
         ,@body))))



(defun format/indent (reporter stream control-string &rest format-arguments)
  "Writes a text to given stream with indentation, dictated by
*indent-level* and *additional-indent*.

If first line start with ~&, then output will start from a fresh line.
Otherwise, all lines except the first one are indented."
  
  (with-slots (indent-space) reporter
    (let ((output (apply #'format nil control-string format-arguments)))
      ;; if string starts with new line, then we have to add indentation
      ;; otherwise we think it is already written to the stream
      (when (ppcre:scan "^~&" control-string)
        (fresh-line stream)
        (format stream (indent indent-space)))

      ;; if this (?!$) is indended to not insert spaces
      ;; into empty lines, then (?m) should be inserted
      ;; before
      ;; TODO: make a pull-request
      (write-string (ppcre:regex-replace-all
                     "(\\n)(?!$)"
                     output
                     (format nil "\\1~A"
                             (indent indent-space)))
                    stream))))


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
