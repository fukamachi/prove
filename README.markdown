# CL-TEST-MORE

CL-TEST-MORE is yet another unit testing framework for Common Lisp, inspired by Test::More, a module of Perl.

The advantages of CL-TEST-MORE are:

* Various simple functions for testing and informative error messages
* Extensible test reporters
* Colorize the report if it's available
* [ASDF integration](#asdf-integration)

## Quickstart

### 1. Writing a test file

```common-lisp
(in-package :cl-user)
(defpackage my-test
  (:use :cl
        :cl-test-more))
(in-package :my-test)

(plan 3)

(ok (not (find 4 '(1 2 3))))
(is 4 4)
(isnt 1 #\1)

(finalize)
```

### 2. Run a test file

```common-lisp
(load #P"/path/to/my-test.lisp")
```

See also: [ASDF integration](#asdf-integration)

### 3. Get a report

![](images/passed.png)

![](images/failed.png)

## Installation

You can install CL-TEST-MORE via [Quicklisp](http://www.quicklisp.org/beta/).

```common-lisp
(ql:quickload :cl-test-more)
```

## Testing functions

### (ok test &optional desc)

Checks if `test` is true (non-NIL).

```common-lisp
(ok 1)
;->  ✓ 1 is expected to be T
```

### (is got expected &rest test-args)

Checks if `got` is equivalent to `expected`.

```common-lisp
(is 1 1)
;->  ✓ 1 is expected to be 1

(is #(1 2 3) #(1 2 3))
;->  × #(1 2 3) is expected to be #(1 2 3)

(is #(1 2 3) #(1 2 3) :test #'equalp)
;->  ✓ #(1 2 3) is expected to be #(1 2 3)

;; with description
(is 1 #\1 "Integer = Character ?")
;->  × Integer = Character ?
```

### (isnt got expected &rest test-args)

Checks if `got` is _not_ equivalent to `expected`.

```common-lisp
(isnt 1 1)
;->  × 1 is not expected to be 1

(isnt #(1 2 3) #(1 2 3))
;->  ✓ #(1 2 3) is not expected to be #(1 2 3)
```

### (is-values got expected &rest test-args)

Checks if the multiple values of `got` is equivalent to `expected`. This is same to `(is (multiple-value-list got) expected)`.

```common-lisp
(defvar *person* (make-hash-table))

(is-values (gethash :name *person*) '("Eitaro" T))
;->  × (NIL NIL) is expected to be ("Eitaro" T)

(setf (gethash :name *person*) "Eitaro")

(is-values (gethash :name *person*) '("Eitaro" T))
;->  ✓ ("Eitaro" T) is expected to be ("Eitaro" T)
```

### (is-type got expected-type &optional desc)

Checks if `got` is a type of `expected-type`.

```common-lisp
(is-type #(1 2 3) 'simple-vector)
;->  ✓ #(1 2 3) is expected to be a type of SIMPLE-VECTOR (got (SIMPLE-VECTOR 3))

(is-type (make-array 0 :adjustable t) 'simple-vector)
;->  × #() is expected to be a type of SIMPLE-VECTOR (got (VECTOR T 0))
```

### (like got regex &optional desc)

Checks if `got` matches a regular expression `regex`.

```common-lisp
(like "Hatsune 39" "\\d")
;->  ✓ "Hatsune 39" is expected to be like "\\d"

(like "初音ミク" "\\d")
;->  × "初音ミク" is expected to be like "\\d"
```

### (is-print got expected &optional desc)

Checks if `got` outputs `expected` to `*standard-output*`

```common-lisp
(is-print (princ "Hi, there") "Hi, there")
;->  ✓ (PRINC "Hi, there") is expected to output "Hi, there" (got "Hi, there")
```

### (is-error form condition &optional desc)

Checks if `form` raises a condition and that is a subtype of `condition`.

```common-lisp
(is-error (error "Something wrong") 'simple-error)
;->  ✓ (ERROR "Something wrong") is expected to raise a condition SIMPLE-ERROR (got #<SIMPLE-ERROR "Something wrong" {100628FE53}>)

(define-condition my-error (simple-error) ())

(is-error (error "Something wrong") 'my-error)
;->  × (ERROR "Something wrong") is expected to raise a condition MY-ERROR (got #<SIMPLE-ERROR "Something wrong" {100648E553}>)
```

### (is-expand got expected &optional desc)

Checks if `got` will be `macroexpand`ed to `expected`.

```common-lisp
(is-expand (when T (princ "Hi")) (if T (progn (princ "Hi"))))
;->  ✓ (WHEN T (PRINC "Hi")) is expected to be expanded to (IF T
;                                                          (PROGN (PRINC "Hi"))) (got (IF T
;                                                                                         (PROGN
;                                                                                          (PRINC
;                                                                                           "Hi"))
;                                                                                         NIL))
```

If a symbol that starts with "$" is contained, it will be treated as a gensym.

### (pass desc)

This will always be passed. This is convenient if the test case is complicated and hard to test with `ok`.

```common-lisp
(pass "Looks good")
;->  ✓ Looks good
```

### (fail desc)

This will always be failed. This is convenient if the test case is complicated and hard to test with `ok`.

```common-lisp
(fail "Hopeless")
;->  × Hopeless
```

### (skip how-many why)

Skip a number of `how-many` tests and mark them passed.

```common-lisp
(skip 3 "No need to test these on Mac OS X")
;->  ✓ No need to test these on Mac OS X (Skipped)
;    ✓ No need to test these on Mac OS X (Skipped)
;    ✓ No need to test these on Mac OS X (Skipped)
```

### (subtest desc &body body)

Run tests of `body` in a new sub test suite.

```common-lisp
(subtest "Testing integers"
  (is 1 1)
  (is-type 1 'bit)
  (is-type 10 'fixnum))
;->      ✓ 1 is expected to be 1
;        ✓ 1 is expected to be a type of BIT (got BIT)
;        ✓ 10 is expected to be a type of FIXNUM (got (INTEGER 0 4611686018427387903))
;->  ✓ Testing integers
```

## Other functions

### (diag desc)

Outputs `desc` to a `*test-result-output*`.

```common-lisp
(diag "Gonna run tests")
;-> # Gonna run tests
```

### (plan num)

Declares a number of `num` tests are going to run. If `finalize` is called with no `plan`, a warning message will be output. `num` is allows to be `NIL` if you have no plan yet.

### (finalize)

Finalizes the current test suite and outputs the test reports.

## Tips

### Colorize test reports on SLIME

SLIME doesn't support to color with ANSI colors in the REPL buffer officially.

You can add the feature by using [slime-repl-ansi-color.el](https://github.com/enriquefernandez/slime-repl-ansi-color).

After installing it, set `cl-test-more:*enable-colors*` to `T` before running tests.

```common-lisp
;; A part of my ~/.sbclrc
(ql:quickload :cl-test-more)
(setf cl-test-more:*enable-colors* t)
```

### ASDF integration

Add `:defsystem-depends-on (:cl-test-more-asdf)` to your testing ASDF system to enable `:test-file` in the `:components`.

`:test-file` is same as `:file` except it will be loaded only when `asdf:test-system`.

```common-lisp
;; Main ASDF system
(defsystem my-app

  ;; ...

  :in-order-to ((test-op (test-op my-app-test))))

;; Testing ASDF system
(defsystem my-app-test
  :depends-on (:my-app
               :cl-test-more)
  :defsystem-depends-on (:cl-test-more-asdf)
  :components
  ((:test-file "my-app"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :cl-test-more)
                             c)))
```

To run tests, execute `asdf:test-system` in your REPL.

```common-lisp
(asdf:test-system :my-app)
```

### Reporters

You can change the test report formats by setting `cl-test-more:*report-style*` to `:list`, `:tap` or `:fiveam`. The default value is `:list`.

### Changing default test function

Test functions like `is` uses `cl-test-more:*default-test-function*` for testing if no `:test` argument is specified. The default value is `#'equal`.

### Changing output stream

Test reports will be output to `cl-test-more:*test-result-output*`. The default value is `T`, which means `*standard-output*`.

## Projects using CL-TEST-MORE

* [Clack](https://github.com/fukamachi/clack)
* [CL-DBI](https://github.com/fukamachi/cl-dbi)
* [datafly](https://github.com/fukamachi/datafly)
* [cl-base58](https://github.com/eudoxia0/cl-base58)
* [Green-Threads](https://github.com/thezerobit/green-threads)

## Bugs

Please report any bugs to e.arrows@gmail.com, or post an issue to [GitHub](http://github.com/fukamachi/cl-test-more/issues).

## License

Copyright (c) 2010-2014 Eitaro Fukamachi &lt;e.arrows@gmail.com&gt;  
CL-TEST-MORE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).

