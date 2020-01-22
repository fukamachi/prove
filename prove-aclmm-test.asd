(defsystem "prove-aclmm-test"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:split-sequence
               :alexandria
               :prove)
  :components ((:module "t"
                :serial t
                :components
                 ((:file "utils")
                  (:test-file "prove-aclmm"))))
  :description "Test system for Prove in Allegro CL modern mode."

  :defsystem-depends-on ("prove-asdf")
  :perform (test-op (o c) (symbol-call :prove-asdf :run-test-system c)))
