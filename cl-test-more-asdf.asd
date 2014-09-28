(in-package :cl-user)
(defpackage cl-test-more-asdf-asd
  (:use :cl :asdf))
(in-package :cl-test-more-asdf-asd)

(defsystem cl-test-more-asdf
    :components ((:module "src"
                  :components
                  ((:file "asdf" :depends-on ("variables"))
                   (:file "variables")))))
