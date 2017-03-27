(defsystem "prove-test"
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

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op (o c) (symbol-call :prove-asdf :run-test-system c)))
