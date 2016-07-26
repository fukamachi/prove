(in-package :cl-user)
(defpackage prove.asdf
  (:nicknames :prove-asdf)
  (:use :cl
        :asdf)
  (:import-from :prove.output
                :*test-result-output*)
  (:import-from :prove.reporter-common
                :*default-reporter*
                :print-failed-reports)
  (:export :test-file
           :run-test-system
           :run

           :*last-suite-report*))
(in-package :prove.asdf)

(defvar *last-suite-report* nil)

(defvar *system-test-files* (make-hash-table))

(defclass test-file (asdf:cl-source-file) ())

(defmethod asdf:perform ((op asdf:compile-op) (c test-file))
  ;; do nothing
  )

#+asdf3
(defmethod asdf::compute-action-stamp :around (plan (o asdf:operation) (c test-file) &key just-done)
  (declare (ignore just-done))
  (let ((*error-output* (make-broadcast-stream)))
    (call-next-method)))

(defmethod asdf:perform ((op asdf:load-op) (c test-file))
  (pushnew c (gethash (asdf:component-system c) *system-test-files*)))

(defun run-test-system (system-designator)
  "Runs a testing ASDF system."
  (setf (gethash (asdf:find-system system-designator) *system-test-files*) '())
  #+quicklisp (ql:quickload (if (typep system-designator 'asdf:system)
                                (asdf:component-name system-designator)
                                system-designator))
  #-quicklisp (asdf:load-system system-designator)
  (let ((passed-files '()) (failed-files '()) (failed-tests '()))
    (restart-case
        (dolist (c (reverse
                    (gethash (asdf:find-system system-designator) *system-test-files*)))
          (setf *last-suite-report* nil)
          (format *test-result-output* "~2&Running a test file '~A'~%" (asdf:component-pathname c))
          (restart-case
              (progn
                (asdf:perform (make-instance 'asdf:load-source-op) c)
                (unless *last-suite-report*
                  (warn "Test completed without 'finalize'd."))
                (if (eql (getf *last-suite-report* :failed) 0)
                    (push (asdf:component-pathname c) passed-files)
                    (let ((pathname (asdf:component-pathname c)))
                      (push pathname failed-files)
                      (push (cons pathname (getf *last-suite-report* :failed-tests))
                            failed-tests))))
            (skip-test-file ()
              :report "Skip this test file."
              (push (asdf:component-pathname c) failed-files))))
      (skip-all-test-files ()
        :report "Give up all test files."
        nil))
    (when failed-tests
      (print-failed-reports nil (nreverse failed-tests) *test-result-output*))
    (values (null failed-files)
            (nreverse passed-files)
            (nreverse failed-files))))

(defun test-files-in-directory (directory)
  (check-type directory pathname)
  (flet ((always-true (&rest args)
           (declare (ignore args))
           T))
    (let ((directories '()))
      (#+asdf3 uiop:collect-sub*directories
       #-asdf3 asdf::collect-sub*directories
       directory
       #'always-true
       #'always-true
       (lambda (dir)
         (push dir directories)))
      (mapcan (lambda (dir)
                (#+asdf3 uiop:directory-files
                 #-asdf3 asdf::directory-files dir "*.lisp"))
              (nreverse directories)))))

(defun run (object &key (reporter *default-reporter*))
  "Runs a test. OBJECT can be one of a file pathname, a directory pathname or an ASDF system name.
Returns 3 multiple-values, a flag if the tests passed as T or NIL, passed test files as a list and failed test files also as a list.

Example:
  (prove:run :myapp-test)
  (prove:run #P\"myapp/tests/\")
  (prove:run #P\"myapp/tests/01-main.lisp\")
"
  (check-type reporter keyword)
  (flet ((directory-pathname-p (path)
           (string= (file-namestring path) "")))
    (let ((*default-reporter* reporter))
      (cond
        ((and (stringp object)
              (asdf:find-system object nil))
         (run-test-system object))
        ((stringp object)
         (run (pathname object)))
        ((and (pathnamep object)
              (directory-pathname-p object))
         (let ((all-passed-p T) (all-passed-files '()) (all-failed-files '())
               (failed-tests '()))
           (restart-case
               (dolist (file (test-files-in-directory object))
                 (multiple-value-bind (passedp passed-files failed-files)
                     (run file)
                   (setf all-passed-files (append all-passed-files passed-files))
                   (setf all-failed-files (append all-failed-files failed-files))
                   (unless passedp
                     (setf all-passed-p nil)
                     (push (cons file (getf *last-suite-report* :failed-tests))
                           failed-tests))))
             (skip-all-test-files ()
               :report "Give up all test files."
               nil))
           (when failed-tests
             (print-failed-reports nil (nreverse failed-tests) *test-result-output*))
           (values all-passed-p all-passed-files all-failed-files)))
        ((pathnamep object)
         (setf *last-suite-report* nil)
         (restart-case
             (progn
               (load object)
               (unless *last-suite-report*
                 (warn "Test completed without 'finalize'd.")))
           (skip-test-file ()
             :report "Skip this test file."
             nil))
         (if (eql (getf *last-suite-report* :failed) 0)
             (values T (list object) '())
             (values NIL '() (list object))))
        (T (run-test-system object))))))

(import 'test-file :asdf)
