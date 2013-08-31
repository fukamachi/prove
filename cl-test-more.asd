(in-package :cl-user)

(defpackage cl-test-more-asd
  (:use :cl :asdf))

(in-package :cl-test-more-asd)

(defsystem cl-test-more
    :version "1.2.0"
    :author "Eitarow Fukamachi"
    :license "MIT"
    :depends-on ("cl-ppcre")
    :components ((:file "cl-test-more")))
