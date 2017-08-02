(in-package :cl-user)
(defpackage t.prove
  (:use :cl
        :prove
        :prove.t.utils))
(in-package :t.prove)

(setf *default-reporter* :list)


(plan 22)

(test-assertion "Successful OK"
                (ok t)
                "✓ t is expected to be t")


(test-assertion "Failed ok without description"
                (ok nil)
                "× nil is expected to be t")


(test-assertion "Failed ok with description"
                (ok nil "This supposed to be failed")
                "
× This supposed to be failed 
  nil is expected to be t")


(test-assertion "Simple number equality check"
                (is 1 1)
                "✓ 1 is expected to be 1")


(test-assertion "String and number shouldn't be equal"
                (is "1" 1)
                "× \"1\" is expected to be 1")


(test-assertion "String and number are not equal and isnt assertion returns OK"
                (isnt "1" 1)
                "✓ \"1\" is not expected to be 1")


(test-assertion "Subtest with diagnostic message"
 (subtest "Subtest"
   (diag "in subtest")
   (is #\a #\a)
   (like "truth" "^true"))
"
Subtest
  in subtest
   ✓ #\\a is expected to be #\\a 
   × \"truth\" is expected to be like \"^true\"")


(test-assertion "Check if (is-values ...) works propertly"
                (is-values (values 1 2 nil 3)
                           '(1 2 nil 3))
                "✓ (1 2 nil 3) is expected to be (1 2 nil 3)")


(test-assertion "Standalone diagnostic message"
                (diag "comment")
                "comment")


(test-assertion "Just a pass"
                (pass "pass")
                "✓ pass")


(test-assertion "Fail"
                (fail "fail")
                "
× fail
  t is expected to be nil")


(test-assertion "Pass with parameter"
                (pass "<~S>")
                "✓ <~S>")


(test-assertion "Equality for strings with formatting"
                (is "<~S>" "<~S>")
                "✓ \"<~S>\" is expected to be \"<~S>\"")


(test-assertion "\"Skip\" with reason as control-string with arguments should substitute arguments"
                (skip 1 "Because ~A" 42)
                "- Because 42 (Skipped)")


(test-assertion "\"Skip\" without reason have default message \"skipping\""
                (skip 1 "skipping")
                "- skipping (Skipped)")


(test-assertion "Assert is-print compares form's output to standart-output"
                (is-print (princ "ABCDEFGH")
                                 "ABCDEFGHIJKLMNO")
                "× (princ \"ABCDEFGH\") is expected to output \"ABCDEFGHIJKLMNO\" (got \"ABCDEFGH\")")


(test-assertion "Type assertion fails if type mismatch"
                (is-type 1 'string)
                "× 1 is expected to be a type of string")


(test-assertion "Assertion \"is-error\" checks if condition of given type was thrown"
                (is-error (error "Raising an error") 'simple-error)
                "(?s)✓ \\(error \"Raising an error\"\\) is expected to raise a condition simple-error \\(got #<(a )?simple-error.*>\\)")


(define-condition my-condition () ())

(test-assertion "If condition type mismatch, \"is-error\" fails"
                (is-error (error 'my-condition) 'simple-error)
                "(?s)× \\(error ('my-condition|\\(quote my-condition\\))\\) is expected to raise a condition simple-error \\(got #<(a t.prove::)?my-condition.*>\\)")


(test-assertion
 "All lines of multiline description should be indented"
 (is 'blah 'blah
     "Blah with multiline
description!")
                "
✓ Blah with multiline
  description!")


(test-assertion
 "Multiline indentation should work for nested tests"
 (subtest "Outer testcase
with multiline
description."
   (is 'blah 'blah
       "Blah with multiline
description!")
  
   (subtest "Inner testcase
with multiline description."
     (is 'foo 'foo
         "Foo with multiline
description!")))
                "
Outer testcase
with multiline
description.
   ✓ Blah with multiline
     description! 
  Inner testcase
  with multiline description.
     ✓ Foo with multiline
       description!")


(test-assertion "Check finalize's output without a plan"
                (finalize)
                "
△ Tests were run but no plan was declared.
✓ 0 tests completed (0ms)")


(finalize)
