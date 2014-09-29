(in-package :cl-user)
(defpackage cl-test-more.asdf
  (:use :cl
        :asdf)
  (:import-from :cl-test-more.output
                :*test-result-output*)
  (:export :test-file
           :run-test-system))
(in-package :cl-test-more.asdf)

(defvar *system-test-files* (make-hash-table))

(defclass test-file (asdf:cl-source-file) ())

(defmethod asdf:perform ((op asdf:compile-op) (c test-file))
  ;; do nothing
  )

#+asdf3
(defmethod asdf::compute-action-stamp :around (plan (o asdf:operation) (c test-file) &key just-done)
  (let ((*error-output* (make-broadcast-stream)))
    (call-next-method)))

(defmethod asdf:perform ((op asdf:load-op) (c test-file))
  (pushnew c (gethash (asdf:component-system c) *system-test-files*)))

(defun run-test-system (system-designator)
  (dolist (c (reverse
              (gethash (asdf:find-system system-designator) *system-test-files*)))
    (format *test-result-output* "~2&Running a test file '~A'~%" (asdf:component-pathname c))
    (asdf:perform (make-instance 'asdf:load-source-op) c)))

(import 'test-file :asdf)
