(in-package :cl-user)
(defpackage cl-test-more
  (:nicknames :test-more)
  (:use :cl)
  (:import-from :cl-test-more.variables
                :*test-result-output*
                :*default-test-function*)
  (:import-from :cl-test-more.asdf
                :test-file
                :run-test-system)
  (:import-from :cl-test-more.test
                :ok
                :is
                :isnt
                :is-values
                :is-print
                :is-error
                :is-type
                :like
                :is-expand
                :diag
                :skip
                :pass
                :fail
                :subtest
                :deftest
                :run-test
                :run-test-package
                :run-test-all
                :remove-test
                :remove-test-all
                :*gensym-prefix*)
  (:import-from :cl-test-more.report
                :*report-style*)
  (:import-from :cl-test-more.suite
                :plan
                :finalize
                :current-suite
                :*suite*
                :reset-suite
                :suite
                :package-suite)
  (:export :*test-result-output*
           :*default-test-function*
           :*report-style*
           :test-file
           :run-test-system
           :ok
           :is
           :isnt
           :is-values
           :is-print
           :is-error
           :is-type
           :like
           :is-expand
           :diag
           :skip
           :pass
           :fail
           :subtest
           :deftest
           :run-test
           :run-test-package
           :run-test-all
           :remove-test
           :remove-test-all
           :plan
           :finalize
           :*gensym-prefix*
           :current-suite
           :*suite*
           :reset-suite
           :suite
           :package-suite))
