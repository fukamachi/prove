(in-package :cl-user)
(defpackage prove-asdf-asd
  (:use :cl :asdf))
(in-package :prove-asdf-asd)

(defsystem prove-asdf
  :components ((:module "src"
                :components
                ((:file "asdf" :depends-on ("output"))
                 (:file "output")))))
