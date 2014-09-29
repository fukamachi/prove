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
                  ((:file "cl-test-more" :depends-on ("test" "suite" "asdf" "color"))
                   (:file "test" :depends-on ("output" "report" "suite"))
                   (:file "report")
                   (:module "report-components"
                    :pathname "report"
                    :depends-on ("report" "color")
                    :components
                    ((:file "tap")
                     (:file "fiveam")
                     (:file "list")))
                   (:file "suite" :depends-on ("output" "report" "asdf"))
                   (:file "asdf" :depends-on ("output"))
                   (:file "color")
                   (:file "output")))))
