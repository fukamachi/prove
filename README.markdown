# CL-TEST-MORE - Yet Another Unit Testing Framework for Common Lisp

CL-TEST-MORE is inspired by Test::More, a module of Perl.

The advantages of CL-TEST-MORE are:

* Just one file to load
* Test with just simple functions
* Results format is "Test Anything Protocol"

## Dependencies

CL-TEST-MORE only works on [Allegro CL](http://www.franz.com/products/allegrocl/) or [SBCL](http://www.sbcl.org/) now.  
Other implementations will be supported soon.

## Synopsis

    (require 'cl-test-more)
    ;; or
    (load "cl-test-more.lisp")
    
    (in-package :cl-test-more)
    
    (plan 9)
    
    ;; check if first argument is true
    (ok (eq got expected) "Description")
    
    ;; check if "got" equals "expected"
    ;; test with `equal'
    (is got expected "Description")
    (isnt got expected "Description")
    
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

## TODO

* Enable to pass a function to test
* Make a description optional
* More portable

## License

Copyright (c) 2010 Eitarow Fukamachi <e.arrows@gmail.com>  
CL-TEST-MORE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
