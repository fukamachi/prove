#|
CL-TEST-MORE - Yet Another Unit Testing Framework for Common Lisp
URL: http://github.com/fukamachi/cl-test-more
Copyright (c) 2010-2011 Eitarow Fukamachi <e.arrows@gmail.com>

CL-TEST-MORE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
|#

(in-package :cl-user)

(defpackage cl-test-more
  (:nicknames :test-more :more)
  (:use :cl :cl-ppcre :annot :annot.doc)
  (:shadow :format))

(in-package :cl-test-more)
(annot:enable-annot-syntax)

(defvar *indent-level* 0)
@export
(defvar *default-test-function* #'equal)
(defvar *tests* nil)
(defvar *gensym-alist* nil)
@export
(defvar *gensym-prefix* "$")
@export
(defvar *test-result-output* t)

(defun write-string/indent (string &optional (stream *standard-output*))
  (write-string
   (make-string (* 4 *indent-level*)
                :initial-element #\Space)
   stream)
  (write-string string stream))

;; (declaim (optimize (debug 3)))

(defun format (stream control-string &rest format-arguments)
  (cond
    ((eq stream *test-result-output*)
     (when (eq stream t)
       (setf stream *standard-output*))
     (map nil
          (lambda (string)
            (write-string/indent string stream)
            (fresh-line stream))
          (ppcre:split "\\n"
           (apply #'cl:format nil control-string format-arguments))))
    (t
     (apply #'cl:format stream control-string format-arguments))))

(defun symbol-package-value (symbol-name &optional default)
  (handler-case
      (symbol-value (intern symbol-name *package*))
    (unbound-variable () default)))

(defun (setf symbol-package-value) (val symbol-name &optional default)
  (declare (ignore default))
  (setf (symbol-value (intern symbol-name *package*)) val))

(defmacro with-package-symbols (symbols &body body)
  `(symbol-macrolet (,@(loop for s in symbols
                             collect
                             (list s
                                   `(symbol-package-value ,(symbol-name s) 0))))
     ,@body))

@export
@doc "I don't know well but it stores the total numbers of test.
It seems to be resetting the counter."
(defun plan (num)
  (with-package-symbols (*plan* *counter* *failed*)
    (setf *plan* num
          *counter* 0
          *failed* 0))
  (when (and num (numberp num))
    (format *test-result-output* "~&1..~a~%" num)))

@export
@doc "Do the test in this package."
(defun finalize ()
  (with-package-symbols (*plan* *counter* *failed*)
    (cond
      ((eq :unspecified
           (handler-case *plan*
             (unbound-variable () :unspecified)))
       (format *test-result-output*
               "~&# Tests were run but no plan was declared.~%"))
      ((and *plan*
            (not (= *counter* *plan*)))
       (format *test-result-output*
               "~&# Looks like you planned ~a tests but ran ~a.~%"
               *plan*
               *counter*)))
    (when (< 0 *failed*)
      (format *test-result-output*
              "~&# Looks like you failed ~a tests of ~a run.~%" *failed* *counter*))

    (prog1
      (= 0 *failed*)
      (setf *plan* :unspecified
            *counter* 0
            *failed* 0))))

(defun add-exit-hook ()
  "DEPRECATED!"
  #+allegro (pushnew '(funcall #'run-test-all) sys:*exit-cleanup-forms*)
  #+sbcl (pushnew #'run-test-all sb-ext:*exit-hooks*)
  #+cmu (pushnew #'run-test-all lisp::*cleanup-functions*)
  #+ccl (pushnew #'run-test-all ccl:*lisp-cleanup-functions*)
  #+ecl (pushnew #'run-test-all si:*exit-hooks*)
  #+clisp (pushnew #'run-test-all custom:*fini-hooks*))

(defun remove-exit-hook ()
  "DEPRECATED!"
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
  (with-package-symbols (*counter* *failed*)
    (incf *counter*)
    (multiple-value-bind (desc arg-test) (parse-description-and-test args)
      (let* ((res (funcall (or test arg-test *default-test-function*) got expected))
             (res (if notp (not res) res)))
        (format *test-result-output*
                "~&~:[not ~;~]ok ~a~:[~;~:* - ~a~]~:[~;~:* # test with ~a~]~%"
                res *counter* desc (function-name test))
        (when (not res)
          (incf *failed*))
        res))))

