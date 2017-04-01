(defsystem "prove-asdf"
  :components ((:module "src"
                :components
                ((:file "asdf" :depends-on ("output"))
                 (:file "output")))))
