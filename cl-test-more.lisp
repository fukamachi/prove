#|
CL-TEST-MORE - Yet Another Unit Testing Framework for Common Lisp
URL: http://github.com/fukamachi/cl-test-more
Copyright (c) 2010 Eitarow Fukamachi <e.arrows@gmail.com>

CL-TEST-MORE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
|#

(in-package :cl-user)

(defpackage cl-test-more
  (:nicknames :test)
  (:use :cl)
  (:export :ok :is :isnt :diag :is-expand :is-print :plan :pass :fail
           :*default-test-function*))

(in-package :cl-test-more)

(defvar *plan* nil)
(defvar *counter* 0)
(defvar *failed* 0)
(defvar *default-test-function* #'equal)

(defun plan (num)
  (setf *plan* num)
  (format t "1..~a~%" num))

(defun finalize ()
  (unless *plan*
    (write-line "# Tests were run but no plan was declared."))
  (when (and *plan* (not (= *counter* *plan*)))
    (format t "# Looks like you planned ~a tests but ran ~a.~%" *plan* *counter*))
  (when *failed*
    (format t "# Looks like you failed ~a tests of ~a run.~%" *failed* *counter*))
  (setf *counter* 0 *failed* 0))

#+allegro (pushnew '(funcall #'finalize) sys:*exit-cleanup-forms*)
#+sbcl (pushnew #'finalize sb-ext:*exit-hooks*)
#+cmu (pushnew #'finalize lisp::*cleanup-functions*)
#+ccl (pushnew #'finalize ccl:*lisp-cleanup-functions*)
#+ecl (pushnew #'finalize si:*exit-hooks*)
#+clisp (pushnew #'finalize custom:*fini-hooks*)

(defun parse-description-and-test (args)
  (if (consp args)
      (case (length args)
        (1 args)
        (2 (if (eq :test (car args))
               (values nil (cadr args))
               (car args)))
        (t (let ((k (member :test args)))
             (case (length k)
               ((0 1) (car args))
               (2 (values (car args) (cadr k)))
               (t (values (nth 2 k) (cadr k)))))))
      args))

(defun function-name (fn)
  #+ccl (ccl:function-name fn)
  #-ccl
  (if fn
      (multiple-value-bind (lambda closurep name) (function-lambda-expression fn)
        (declare (ignore lambda closurep))
        name)))

(defun test (got expected args &key notp)
  (incf *counter*)
  (multiple-value-bind (desc test) (parse-description-and-test args)
    (let* ((res (funcall (or test *default-test-function*) got expected))
           (res (if notp (not res) res)))
      (format t "~:[not ~;~]ok ~a~:[~;~:* - ~a~]~:[~;~:* # test with ~a~]~%" res *counter* desc (function-name test))
      (when (not res)
        (incf *failed*))
      res)))

(defun ok (test &optional desc)
  (test (not (null test)) t desc))

(defun is (got expected &rest args)
  (or (test got expected args)
      (format t "#   got: ~S~%#   expected: ~S~%" got expected)))

(defun isnt (got expected &rest args)
  (or (test got expected args :notp t)
      (format t "#   got: ~S~%#   not expected: ~S~%" got expected)))

(defun diag (desc)
  (format t "# ~a~%" desc))

(defmacro is-expand (got expected &optional desc)
  (let ((expanded (gensym)))
    `(let ((,expanded (macroexpand-1 ',got)))
       (or (test ,expanded ',expected ,desc)
           (format t "#   got: ~S~%#   expanded: ~S~%#   expected: ~S~%"
                   ',got ,expanded ',expected)))))

(defmacro is-print (got expected &optional desc)
  (let ((res (gensym)))
    `(let ((,res (with-output-to-string (*standard-output*) ,got)))
       (test ,res ,expected ,desc))))

(defun pass (desc &rest args)
  (test t t (apply #'format nil desc args)))

(defun fail (desc &rest args)
  (test t nil (apply #'format nil desc args)))
