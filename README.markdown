# prove

_This project was originally called 'CL-TEST-MORE'._

'prove' is yet another unit testing framework for Common Lisp.

The advantages of 'prove' are:

* Various simple functions for testing and informative error messages
* [ASDF integration](#asdf-integration)
* [Extensible test reporters](#reporters)
* Colorizes the report if it's available ([note for SLIME](#colorize-test-reports-on-slime))
* Reports test durations

## Quickstart

### 1. Writing a test file

```common-lisp
(in-package :cl-user)
(defpackage my-test
  (:use :cl
        :prove))
(in-package :my-test)

(plan 3)

(ok (not (find 4 '(1 2 3))))
(is 4 4)
(isnt 1 #\1)

(finalize)
```

### 2. Run a test file

```common-lisp
(prove:run #P"myapp/tests/my-test.lisp")
(prove:run #P"myapp/tests/my-test.lisp" :reporter :list)
```

See also: [ASDF integration](#asdf-integration), [Reporters](#reporters)

### 3. Get a report

![](images/passed.png)

![](images/failed.png)

## Installation

You can install 'prove' via [Quicklisp](http://www.quicklisp.org/beta/).

```common-lisp
(ql:quickload :prove)
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

### (slow-threshold milliseconds)

Set the threshold of slow test durations for the current test suite. The default threshold value is `prove:*default-slow-threshold*`.

```common-lisp
(slow-threshold 150)
```

## Reporters

You can change the test report formats by setting `prove:*default-reporter*` to `:list`, `:dot`, `:tap` or `:fiveam`. The default value is `:list`.

`prove:run` also takes a keyword argument `:reporter`.

### List (Default)

The `:list` repoter outputs test results list as test cases pass or fail.

![](images/list.png)

### Dot

The `:dot` reporter outputs a series of dots that represent test cases, failures highlight in red, skipping in cyan.

![](images/dot.png)

### FiveAM

The `:fiveam` reporter outputs test results like [FiveAM](http://common-lisp.net/project/fiveam/) does.

![](images/fiveam.png)

### TAP

The `:tap` reporter outputs in [Test Anything Protocol](http://testanything.org) format.

![](images/tap.png)

## Tips

### Debugging with CL debugger

Set `prove:*debug-on-error*` T for invoking CL debugger whenever getting an error during running tests.

### Colorize test reports on SLIME

SLIME doesn't support to color with ANSI colors in the REPL buffer officially.

You can add the feature by using [slime-repl-ansi-color.el](https://github.com/enriquefernandez/slime-repl-ansi-color).

After installing it, set `prove:*enable-colors*` to `T` before running tests.

```common-lisp
;; A part of my ~/.sbclrc
(ql:quickload :prove)
(setf prove:*enable-colors* t)
```

The following snippet is a little bit complicated, however it would be better if you don't like to load `prove` in all sessions.

```common-lisp
(defmethod asdf:perform :after ((op asdf:load-op) (c (eql (asdf:find-system :prove))))
  (setf (symbol-value (intern (string :*enable-colors*) :prove)) t))
```

### ASDF integration

Add `:defsystem-depends-on (:prove-asdf)` to your testing ASDF system to enable `:test-file` in the `:components`.

`:test-file` is same as `:file` except it will be loaded only when `asdf:test-system`.

```common-lisp
;; Main ASDF system
(defsystem my-app

  ;; ...

  :in-order-to ((test-op (test-op my-app-test))))

;; Testing ASDF system
(defsystem my-app-test
  :depends-on (:my-app
               :prove)
  :defsystem-depends-on (:prove-asdf)
  :components
  ((:test-file "my-app"))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
```

To run tests, execute `asdf:test-system` or `prove:run` in your REPL.

```common-lisp
(asdf:test-system :my-app)
(asdf:test-system :my-app-test)

;; Same as 'asdf:test-system' except it returns T or NIL as the result of tests.
(prove:run :my-app-test)
```

### Changing default test function

Test functions like `is` uses `prove:*default-test-function*` for testing if no `:test` argument is specified. The default value is `#'equal`.

### Changing output stream

Test reports will be output to `prove:*test-result-output*`. The default value is `T`, which means `*standard-output*`.

### Running tests on Travis CI

Although Common Lisp isn't supported by Travis CI officially, you can run tests by using [cl-travis](https://github.com/luismbo/cl-travis).

Here's a list of `.travis.yml` from projects using `prove` on Travis CI:

- [Clack](https://github.com/fukamachi/clack/blob/master/.travis.yml)
- [CL-DBI](https://github.com/fukamachi/cl-dbi/blob/master/.travis.yml)
- [Woo](https://github.com/fukamachi/Woo/blob/master/.travis.yml)
- [fast-http](https://github.com/fukamachi/fast-http/blob/master/.travis.yml)
- [defclass-std](https://github.com/EuAndreh/defclass-std/blob/master/.travis.yml)

## Bugs

Please report any bugs to e.arrows@gmail.com, or post an issue to [GitHub](http://github.com/fukamachi/prove/issues).

## License

Copyright (c) 2010-2014 Eitaro Fukamachi &lt;e.arrows@gmail.com&gt;  
'prove' and CL-TEST-MORE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
