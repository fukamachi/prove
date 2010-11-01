# CL-TEST-MORE - Yet Another Unit Testing Framework for Common Lisp

CL-TEST-MORE is inspired by Test::More, a module of Perl.

The advantages of CL-TEST-MORE are:

* Just one file to load
* Test with just simple functions
* Test as a script
* Results format is "Test Anything Protocol"

## Dependencies

CL-TEST-MORE is almost written in portable Common Lisp code.  
But, a feature, tests as a script is only supported Allegro CL, SBCL, CMUCL, Clozure CL, ECL and CLISP.  
If you use other implementation, you have to put <code>(finalize)</code> at the end of file.

## Synopsis

    (require 'cl-test-more)
    ;; or
    (load "cl-test-more.lisp")
    
    (in-package :cl-test-more)
    
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

## Installation

    (require 'asdf)
    (require 'asdf-install)
    (asdf-install:install "http://github.com/fukamachi/cl-test-more/tarball/master")

## How to run tests

### Allegro CL

    $ alisp -#! filename.lisp

### SBCL

    $ sbcl --script filename.lisp

### CMUCL

    $ cmucl -load filename.lisp -eval '(quit)'

### Clozure CL

    $ ccl --load filename.lisp --eval '(quit)'

### ECL

    $ ecl -shell filename.lisp

### CLISP

    $ clisp filename.lisp

## Bugs

Please report any bugs to e.arrows@gmail.com, or post an issue to [GitHub](http://github.com/fukamachi/cl-test-more/issues).

## License

Copyright (c) 2010 Eitarow Fukamachi &lt;e.arrows@gmail.com&gt;  
CL-TEST-MORE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
