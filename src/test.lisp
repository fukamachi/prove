(in-package :cl-user)
(defpackage prove.test
  (:use :cl)
  (:import-from :prove.output
                :*test-result-output*)
  (:import-from :prove.report
                :test-report-p
                :passed-test-report
                :failed-test-report
                :error-test-report
                :skipped-test-report
                :comment-report
                :composed-test-report
                :failed-report-p
                :duration)
  (:import-from :prove.reporter
                :format-report
                :*indent-level*
                :*additional-indent*)
  (:import-from :prove.suite
                :suite
                :*suite*
                :suite-plan
                :test-count
                :failed
                :reports
                :slow-threshold
                :current-suite
                :finalize
                :add-report)
  (:import-from :alexandria
                :with-gensyms
                :once-only)
  (:export :*default-test-function*
           :*debug-on-error*

           :ok
           :is
           :isnt
           :is-values
           :is-print
           :is-condition
           :is-error
           :is-type
           :like
           :is-expand
           :diag
           :skip
           :pass
           :fail
           :subtest

           :*gensym-prefix*

           :deftest
           :run-test
           :run-test-package
           :run-test-all
           :remove-test
           :remove-test-all))
(in-package :prove.test)

(defvar *debug-on-error* nil)
(defvar *default-test-function* #'equal)

(defun parse-description-and-test (args)
  (if (consp args)
      (case (length args)
        (1 (car args))
        (2 (if (eq :test (car args))
               (values nil (cadr args))
               (car args)))
        (t (let ((k (member :test args)))
             (case (length k)
               ((0 1) (car args))
               (2 (values (car args) (cadr k)))
               (t (values (nth 2 k) (cadr k)))))))
      args))

(defun test (got expected args
             &key notp
               duration
               (got-form nil got-form-supplied-p)
               (test-fn *default-test-function*)
               (passed-report-class 'passed-test-report)
               (failed-report-class 'failed-test-report)
               report-expected-label
               (print-error-detail t)
               (output t))
  (multiple-value-bind (desc arg-test)
      (parse-description-and-test args)
    (let* ((test-function (or arg-test test-fn))
           (result (funcall test-function got expected))
           (result (if notp (not result) result))
           (suite (current-suite))
           (report (apply #'make-instance
                          (if result
                              passed-report-class
                              failed-report-class)
                          :duration duration
                          :slow-threshold (slot-value suite 'slow-threshold)
                          :test-function test-function
                          :notp notp
                          :got got
                          :got-form (if got-form-supplied-p
                                        got-form
                                        got)
                          :expected expected
                          :description desc
                          :print-error-detail print-error-detail
                          (and report-expected-label
                               (list :report-expected-label report-expected-label)))))
      (add-report report suite)
      (unless result
        (incf (failed suite)))
      (incf (test-count suite))
      (when output
        (format-report *test-result-output* nil report :count (test-count suite)))
      (values result report))))

(defmacro with-duration (((duration result) form) &body body)
  (with-gensyms (start end)
    `(let* ((,start (get-internal-real-time))
            (,result ,form)
            (,end (get-internal-real-time))
            (,duration (- ,end ,start)))
       ,@body)))

(defmacro with-catching-errors ((&key description expected) &body body)
  (with-gensyms (e suite report)
    `(if *debug-on-error*
         (progn ,@body)
         (handler-case (progn ,@body)
           (error (,e)
             (let ((,suite (current-suite))
                   (,report (make-instance 'error-test-report
                                           :got ,e
                                           :got-form ,e
                                           :expected ,expected
                                           :description ,description
                                           :duration nil)))
               (add-report ,report ,suite)
               (incf (failed ,suite))
               (incf (test-count ,suite))
               (format-report *test-result-output* nil ,report :count (test-count ,suite))))))))

(defmacro ok (test &optional desc)
  (with-gensyms (duration result)
    (once-only (test desc)
      `(with-catching-errors (:expected T :description ,desc)
         (with-duration ((,duration ,result) ,test)
           (test ,result t ,desc
                 :duration ,duration
                 :test-fn (lambda (x y)
                            (eq (not (null x)) y))
                 :got-form ,test))))))

(defmacro is (got expected &rest args)
  (with-gensyms (duration result new-args desc)
    (once-only (expected)
      `(let* ((,new-args (list ,@args))
              (,desc (parse-description-and-test ,new-args)))
         (with-catching-errors (:description ,desc :expected ,expected)
           (with-duration ((,duration ,result) ,got)
             (test ,result ,expected ,new-args
                   :duration ,duration)))))))

(defmacro isnt (got expected &rest args)
  (with-gensyms (duration result new-args desc)
    (once-only (expected)
      `(let* ((,new-args (list ,@args))
              (,desc (parse-description-and-test ,new-args)))
         (with-catching-errors (:description ,desc :expected ,expected)
           (with-duration ((,duration ,result) ,got)
             (test ,result ,expected ,new-args
                   :notp t
                   :duration ,duration)))))))

(defmacro is-values (got expected &rest args)
  `(is (multiple-value-list ,got) ,expected ,@args))

(defmacro is-print (got expected &optional desc)
  (with-gensyms (output duration duration-inner)
    (once-only (expected desc)
      `(with-catching-errors (:description ,desc :expected ,expected)
         (let* (,duration
                (,output (with-output-to-string (*standard-output*)
                           (with-duration ((,duration-inner ,output) ,got)
                             (declare (ignore ,output))
                             (setq ,duration ,duration-inner)))))
           (test ,output ,expected ,desc
                 :duration ,duration
                 :got-form ',got
                 :test-fn #'string=
                 :report-expected-label "output"))))))

(defmacro is-condition (form condition &optional desc)
  (with-gensyms (error duration)
    `(with-duration ((,duration ,error) (handler-case ,form
                                          (condition (,error) ,error)))
       (test ,error
             ,(if (and (listp condition) (eq 'quote (car condition)))
                  condition
                  `(quote ,condition))
             ,desc
             :duration ,duration
             :got-form ',form
             :test-fn #'typep
             :report-expected-label "raise a condition"))))

;;; alias is-error to is-condition
(setf (macro-function 'is-error) (macro-function 'is-condition))

(defmacro is-type (got expected-type &optional desc)
  (with-gensyms (duration result)
    (once-only (desc expected-type)
      `(with-catching-errors (:description ,desc :expected ,expected-type)
         (with-duration ((,duration ,result) ,got)
           (test ,result ,expected-type ,desc
                 :duration ,duration
                 :got-form ',got
                 :test-fn #'typep
                 :report-expected-label "be a type of"))))))

(defmacro like (got regex &optional desc)
  (with-gensyms (duration result)
    (once-only (regex desc)
      `(with-catching-errors (:description ,desc :expected ,regex)
         (with-duration ((,duration ,result) ,got)
           (test ,result ,regex ,desc
                 :duration ,duration
                 :test-fn (lambda (x y) (not (null (ppcre:scan y x))))
                 :report-expected-label "be like"))))))

(defvar *gensym-prefix* "$")
(defvar *gensym-alist* nil)

(defun gensymp (val)
  (and (symbolp val)
       (string= (subseq (symbol-name val) 0 (length *gensym-prefix*)) *gensym-prefix*)))

(defgeneric gensym-tree-equal (x y)
  (:method (x y)
    (if (and (gensymp y) (symbolp x))
        (if (assoc y *gensym-alist*)
            (eq x (cdr (assoc y *gensym-alist*)))
            (unless (rassoc x *gensym-alist*)
              (setf *gensym-alist* `((,y . ,x) ,@*gensym-alist*))
              t))
        (equal x y)))
  (:method ((x cons) (y cons))
    (loop for a in x for b in y
          always (gensym-tree-equal a b))))

(defmacro is-expand (got expected &optional desc)
  (with-gensyms (duration expanded)
    (once-only (desc)
      `(with-duration ((,duration ,expanded) (macroexpand-1 ',got))
         (let (*gensym-alist*)
           (test ,expanded ',expected ,desc
                 :duration ,duration
                 :got-form ',got
                 :report-expected-label "be expanded to"
                 :test-fn #'gensym-tree-equal))))))

(defun diag (desc)
  (let ((report (make-instance 'comment-report
                               :description desc)))
    (add-report report (current-suite))
    (format-report *test-result-output* nil report)))

(defun skip (how-many why &rest format-args)
  (check-type how-many integer)
  (dotimes (i how-many)
    (test t t (apply #'format nil why format-args)
      :passed-report-class 'skipped-test-report)))

(defun pass (desc)
  (test t t desc))

(defun fail (desc)
  (test t nil desc
        :print-error-detail nil))

(defun %subtest (desc body-fn)
  (diag desc)
  (let ((report
          (let ((*suite* (make-instance 'suite))
                (*indent-level* (1+ *indent-level*))
                (*additional-indent* 0))
            (if *debug-on-error*
                (funcall body-fn)
                (handler-case (funcall body-fn)
                  (error (e)
                    (let ((error-report
                            (make-instance 'error-test-report
                                           :expected :non-error
                                           :got e
                                           :description (format nil "Aborted due to an error in subtest ~S" desc))))
                      (add-report error-report *suite*)
                      (format-report *test-result-output* nil error-report :count (test-count *suite*))))))
            (make-instance 'composed-test-report
                           :duration (reduce #'+
                                             (remove-if-not #'test-report-p (reports *suite*))
                                             :key (lambda (report) (or (slot-value report 'duration) 0)))
                           :plan (suite-plan *suite*)
                           :description desc
                           :children (reports *suite*))))
        (suite (current-suite)))
    (add-report report suite)
    (incf (test-count suite))
    (format-report *test-result-output* nil report :count (test-count suite))))

(defmacro subtest (desc &body body)
  `(%subtest ,desc (lambda () ,@body)))

(defvar *package-tests* (make-hash-table))

(defmacro deftest (name &body test-forms)
  (let ((tests (gensym "TESTS"))
        (test (gensym "TEST"))
        (test-fn (gensym "TEST-FN")))
    `(progn
       (unless (nth-value 1 (gethash *package* *package-tests*))
         (setf (gethash *package* *package-tests*) '()))
       (let* ((,tests (gethash *package* *package-tests*))
              (,test (assoc ',name ,tests :test #'string=))
              (,test-fn (lambda ()
                          (subtest (princ-to-string ',name)
                            ,@test-forms))))
         (if ,test
             (rplacd ,test ,test-fn)
             (push (cons ',name ,test-fn) (gethash *package* *package-tests*)))
         ',name))))

(defun run-test (name)
  (let ((test (assoc name
                     (gethash *package* *package-tests*)
                     :test #'string=)))
    (unless test
      (error "Test not found: ~S" name))
    (funcall (cdr test))))

(defun run-test-package (package-designator)
  (let ((*package* (typecase package-designator
                     (package package-designator)
                     (T (find-package package-designator)))))
    (loop for (name . test-fn) in (reverse (gethash *package* *package-tests*))
          do (funcall test-fn))
    (finalize)))

(defun run-test-all ()
  (maphash (lambda (package tests)
             (declare (ignore tests))
             (run-test-package package))
           *package-tests*))

(defun remove-test (name)
  (setf (gethash *package* *package-tests*)
        (delete name
                (gethash *package* *package-tests*)
                :key #'car
                :test #'string=)))

(defun remove-test-all ()
  (setf (gethash *package* *package-tests*) nil))
