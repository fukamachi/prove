(in-package :cl-user)

(defpackage cl-test-more-asd
  (:use :cl :asdf))

(in-package :cl-test-more-asd)

(defsystem cl-test-more
    :version "1.2.1"
    :author "Eitarow Fukamachi"
    :license "MIT"
    :depends-on ("cl-ppcre")
    :components ((:module "src"
                  :components
                  ((:file "cl-test-more" :depends-on ("asdf"))
                   (:file "asdf")))))
