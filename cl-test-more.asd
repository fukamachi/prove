(in-package :cl-user)

(defpackage cl-test-more-asd
  (:use :cl :asdf))

(in-package :cl-test-more-asd)

(defsystem cl-test-more
    :version "1.1.0-SNAPSHOT"
    :author "Eitarow Fukamachi"
    :license "MIT"
    :depends-on ("cl-ppcre")
    :components ((:file "cl-test-more")))
