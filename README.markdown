# CL-TEST-MORE - Yet Another Unit Testing Framework for Common Lisp

CL-TEST-MORE is inspired by Test::More, a module of Perl.

The advantages of CL-TEST-MORE are:

* need to load just one file
* test with just simple functions
* report results in "Test Anything Protocol" format

## Synopsis

```common-lisp
(plan 9)

;; Various way to say "ok":
;; by checking if the first argument is true,
(ok (eq got expected) "the name or description of this test")

;; by checking if the value of `got' equals the value of `expected', and
(is got expected "the name or description of this test")
(isnt got expected "the name or description of this test")
;; with :test function.
(is got expected "the name or description of this test" :test #'string=)

;; Rather than (princ "# This is just a comment\n" *standard-output*)
(diag "This is just a comment")

;; Check a macro expansion.
(is-expand (got macro) (expected :like "this") "the name or description of this test")

;; Check an output.
(is-print (write-line "aiueo") "aiueo\n" "the name or description of this test")

;; A function always passes, and always fails.
(pass "the name or description of this test")
(fail "the name or description of this test")

;; Don't forget this.
(finalize)
```

## Installation

### Quicklisp

```common-lisp
(ql:quickload :cl-test-more)
```

### ASDF-Install

```common-lisp
(asdf-install:install "http://github.com/fukamachi/cl-test-more/tarball/master")
```

## Functions

* ok
* is
* isnt
* diag
* is-expand
* is-values
* is-print
* is-error
* is-type
* like
* skip
* pass
* fail

* deftest
* run-test
* run-test-all

## Change default test function

## Test Macros

## Change output stream

## Bugs

Please report any bugs to e.arrows@gmail.com, or post an issue to [GitHub](http://github.com/fukamachi/cl-test-more/issues).

## License

Copyright (c) 2010-2014 Eitarow Fukamachi &lt;e.arrows@gmail.com&gt;  
CL-TEST-MORE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
