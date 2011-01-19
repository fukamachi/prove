#|
CL-TEST-MORE - Yet Another Unit Testing Framework for Common Lisp
URL: http://github.com/fukamachi/cl-test-more
Copyright (c) 2010-2011 Eitarow Fukamachi <e.arrows@gmail.com>

CL-TEST-MORE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
|#

(in-package :cl-user)

(defpackage cl-test-more
  (:nicknames :test)
  (:use :cl)
  (:export :ok :is :isnt :diag :is-expand :is-print :is-error :plan :pass :fail
           :finalize
           :deftest :run-test :run-test-all
           :remove-test :remove-test-all
           :*default-test-function*))

(in-package :cl-test-more)

(defvar *plan* nil)
(defvar *counter* 0)
(defvar *failed* 0)
(defvar *default-test-function* #'equal)
(defvar *tests* nil)

(defun plan (num)
  (setf *plan* num)
  (format t "1..~a~%" num))

(defun finalize ()
  (format t "~&~%")
  (unless *plan*
    (write-line "# Tests were run but no plan was declared."))
  (when (and *plan* (not (= *counter* *plan*)))
    (format t "# Looks like you planned ~a tests but ran ~a.~%" *plan* *counter*))
  (when (< 0 *failed*)
    (format t "# Looks like you failed ~a tests of ~a run.~%" *failed* *counter*))
  (setf *plan* 0 *counter* 0 *failed* 0))

(defun add-exit-hook ()
  #+allegro (pushnew '(funcall #'run-test-all) sys:*exit-cleanup-forms*)
  #+sbcl (pushnew #'run-test-all sb-ext:*exit-hooks*)
  #+cmu (pushnew #'run-test-all lisp::*cleanup-functions*)
  #+ccl (pushnew #'run-test-all ccl:*lisp-cleanup-functions*)
  #+ecl (pushnew #'run-test-all si:*exit-hooks*)
  #+clisp (pushnew #'run-test-all custom:*fini-hooks*))

(defun remove-exit-hook ()
  (macrolet ((delete! (item seq)
               `(setf ,seq (delete ,item ,seq :test #'equal))))
    #+allegro (delete! '(funcall #'run-test-all) sys:*exit-cleanup-forms*)
    #+sbcl (delete! #'run-test-all sb-ext:*exit-hooks*)
    #+cmu (delete! #'run-test-all lisp::*cleanup-functions*)
    #+ccl (delete! #'run-test-all ccl:*lisp-cleanup-functions*)
    #+ecl (delete! #'run-test-all si:*exit-hooks*)
    #+clisp (delete! #'run-test-all custom:*fini-hooks*)))

(defun parse-description-and-test (args)
  (if (consp args)
      (case (length args)
        (1 (car args))
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

(defun test (got expected args &key notp test)
  (incf *counter*)
  (multiple-value-bind (desc arg-test) (parse-description-and-test args)
    (let* ((res (funcall (or test arg-test *default-test-function*) got expected))
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
       (or (test ,expanded ',expected ,desc :test #'equal)
           (format t "#   got: ~S~%#   expanded: ~S~%#   expected: ~S~%"
                   ',got ,expanded ',expected)))))

(defmacro is-print (got expected &optional desc)
  (let ((res (gensym)))
    `(let ((,res (with-output-to-string (*standard-output*) ,got)))
       (test ,res ,expected ,desc))))

(defmacro is-error (form condition &optional desc)
  (let ((err (gensym)))
    `(let ((,err (handler-case ,form
                   (condition (error) error))))
       (or (test (typep ,err ',condition) t ,desc)
           (format t "#   got: ~S~%#   expected error: ~S~%"
                   ,err ',condition)))))

(defun pass (desc &rest args)
  (test t t (apply #'format nil desc args)))

(defun fail (desc &rest args)
  (test t nil (apply #'format nil desc args)))

(defun find-test (name)
  (assoc (symbol-name name) *tests* :test #'string=))

(defmacro deftest (name &rest test-forms)
  (let ((test (gensym)))
    `(let ((,test (find-test ',name)))
       (if ,test
           (rplacd ,test (lambda () ,@test-forms))
           (push (cons ,(symbol-name name)
                       (lambda () ,@test-forms))
                 *tests*)))))

(defun run-test (name &key (finalizep t))
  (format t "~&~%# Test: ~a~%" name)
  (let ((test (find-test name)))
    (if test
        (funcall (cdr test))
        (error "Not found test: ~a" (car test))))
  (and finalizep (finalize))
  (remove-exit-hook))

(defun run-test-all ()
  (map nil (lambda (test) (run-test (intern (car test)) :finalizep nil)) (reverse *tests*))
  (finalize))

(defun remove-test (name)
  (setf *tests* (delete name *tests* :key #'car)))

(defun remove-test-all ()
  (setf *tests* nil))

(add-exit-hook)
