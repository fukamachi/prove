(in-package :cl-user)
(defpackage prove-test-asd
  (:use :cl :asdf))
(in-package :prove-test-asd)


(defsystem prove-test
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:split-sequence
               :alexandria
               :prove)
  :components ((:module "t"
                :serial t
                :components
                 ((:file "utils")
                  (:test-file "prove"))))
  :description "Test system for Prove."

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
