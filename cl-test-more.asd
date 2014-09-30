(in-package :cl-user)
(defpackage cl-test-more-asd
  (:use :cl :asdf))
(in-package :cl-test-more-asd)

(defsystem cl-test-more
  :version "2.0.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:prove))
