(in-package :cl-user)
(defpackage prove
  (:nicknames :cl-test-more :test-more)
  (:use :cl)
  (:import-from :prove.output
                :*test-result-output*)
  (:import-from :prove.asdf
                :test-file
                :run-test-system)
  (:import-from :prove.test
                :*default-test-function*
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
  (:import-from :prove.reporter
                :*report-style*)
  (:import-from :prove.suite
                :*default-slow-threshold*
                :slow-threshold
                :plan
                :finalize
                :current-suite
                :*suite*
                :reset-suite
                :suite
                :package-suite)
  (:import-from :prove.color
                :*enable-colors*)
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
           :*default-slow-threshold*
           :slow-threshold
           :current-suite
           :*suite*
           :reset-suite
           :suite
           :package-suite
           :*enable-colors*))
