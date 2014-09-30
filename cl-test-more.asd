(in-package :cl-user)
(defpackage cl-test-more-asd
  (:use :cl :asdf))
(in-package :cl-test-more-asd)

(defsystem cl-test-more
    :version "2.0.0"
    :author "Eitaro Fukamachi"
    :license "MIT"
    :depends-on (:cl-ppcre
                 :cl-ansi-text
                 :alexandria)
    :components ((:module "src"
                  :components
                  ((:file "cl-test-more" :depends-on ("test" "suite" "reporter" "asdf" "color"))
                   (:file "test" :depends-on ("output" "report" "reporter" "suite"))
                   (:file "report")
                   (:file "reporter" :depends-on ("report"))
                   (:module "reporter-components"
                    :pathname "reporter"
                    :depends-on ("report" "reporter")
                    :components
                    ((:file "tap")
                     (:file "fiveam")
                     (:file "list" :depends-on ("color"))))
                   (:file "suite" :depends-on ("output" "report" "reporter" "asdf"))
                   (:file "asdf" :depends-on ("output"))
                   (:file "color")
                   (:file "output")))))
