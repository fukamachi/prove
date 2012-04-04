# CL-TEST-MORE - Yet Another Unit Testing Framework for Common Lisp

CL-TEST-MORE is inspired by Test::More, a module of Perl.

The advantages of CL-TEST-MORE are:

* Just one file to load
* Test with just simple functions
* Results format is "Test Anything Protocol"

## Synopsis

```common-lisp
(plan 9)

;; check if first argument is true
(ok (eq got expected) "Description")

;; check if "got" equals "expected"
(is got expected "Description")
(isnt got expected "Description")
;; with :test function
(is got expected "Description" :test #'string=)

;; rather than print *standard-output* "# This is just a comment\n"
(diag "This is just a comment")

;; macro expansion
(is-expand (got macro) (expected :like "this") "Description")

;; output
(is-print (write-line "aiueo") "aiueo\n" "Description")

;; functions always pass or fail
(pass "Description")
(fail "Description")

;; Don't forget this
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

Copyright (c) 2010-2011 Eitarow Fukamachi &lt;e.arrows@gmail.com&gt;  
CL-TEST-MORE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
