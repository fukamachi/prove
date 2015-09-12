(in-package :cl-user)
(defpackage prove
  (:nicknames :cl-test-more :test-more)
  (:use :cl)
  (:import-from :prove.output
                :*test-result-output*
                :*default-reporter*)
  (:import-from :prove.asdf
                :test-file
                :run-test-system
                :run)
  (:import-from :prove.test
                :*debug-on-error*
                :*default-test-function*
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
                :deftest
                :run-test
                :run-test-package
                :run-test-all
                :remove-test
                :remove-test-all
                :*gensym-prefix*)
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
  (:export :*debug-on-error*
           :*test-result-output*
           :*default-test-function*
           :*default-reporter*
           :test-file
           :run-test-system
           :run
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
