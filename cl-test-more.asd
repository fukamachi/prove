(in-package :cl-user)
(defpackage cl-test-more-asd
  (:use :cl :asdf))
(in-package :cl-test-more-asd)

(defsystem cl-test-more
    :version "2.0.0"
    :author "Eitaro Fukamachi"
    :license "MIT"
    :depends-on (:cl-ppcre)
    :components ((:module "src"
                  :components
                  ((:file "cl-test-more" :depends-on ("test" "suite" "asdf"))
                   (:file "test" :depends-on ("variables" "report" "suite"))
                   (:file "report" :depends-on ("variables"))
                   (:module "report-components"
                    :pathname "report"
                    :depends-on ("report")
                    :components
                    ((:file "tap")
                     (:file "fiveam")))
                   (:file "suite" :depends-on ("variables" "report"))
                   (:file "asdf" :depends-on ("variables"))
                   (:file "variables")))))
