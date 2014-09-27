(in-package :cl-user)
(defpackage cl-test-more.asdf
  (:use :cl
        :asdf)
  (:export :test-file
           :run-test-system))
(in-package :cl-test-more.asdf)

(defvar *system-test-files* (make-hash-table))

(defclass test-file (asdf:cl-source-file) ())

(defmethod asdf:perform ((op asdf:compile-op) (c test-file))
  ;; do nothing
  )

(defmethod asdf:perform ((op asdf:load-op) (c test-file))
  (pushnew c (gethash (asdf:component-system c) *system-test-files*)))

(defun run-test-system (system-designator)
  (dolist (c (reverse
              (gethash (asdf:find-system system-designator) *system-test-files*)))
    (asdf:perform (make-instance 'asdf:load-source-op) c)))

(import 'test-file :asdf)
