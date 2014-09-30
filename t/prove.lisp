(in-package :cl-user)
(defpackage t.prove
  (:use :cl
        :prove))
(in-package :t.prove)

(setf *default-reporter* :list)

(plan 10)

(ok t)
(ok nil "This supposed to be failed")
(is 1 1)
(is "1" 1)
(isnt "1" 1)

(subtest "Subtest"
  (diag "in subtest")
  (is #\a #\a)
  (like "truth" "^true"))

(is-values (values 1 2 nil 3)
           '(1 2 nil 3))

(diag "comment")

(pass "pass")
(fail "fail")
(pass "<~S>")
(is "<~S>" "<~S>")
(skip 1 "~A" 1)

(skip 1 "skipping")

(is-print (princ "AAAAAAAAAA")
          "AAAAAAAAAAAAA")

(is-type 1 'string)

(is-error (error "Raising an error") 'simple-error)
(define-condition my-condition () ())
(is-error (error 'my-condition) 'simple-error)

(finalize)
