#|
CL-TEST-MORE - Yet Another Unit Testing Framework for Common Lisp
URL: http://github.com/fukamachi/cl-test-more
Copyright (c) 2010-2011 Eitarow Fukamachi <e.arrows@gmail.com>

CL-TEST-MORE is freely distributable under the MIT License (http://www.opensource.org/licenses/mit-license).
|#

(in-package :cl-user)

(defpackage cl-test-more
  (:nicknames :test-more)
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
@doc "Finalizes the tests defined in the current package.
All test defined in this package become accessible from outside
via `find-tests-of-package'."
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

@export
@doc "tests if the first argument is non-nil"
(defun ok (test &optional desc)
  (test (not (null test)) t desc))

@export
@doc "tests if the first argument is nil"
(defun ng (test &optional desc)
  (test (not (null test)) t desc :notp t))

@export
@doc "tests (equal got expected)"
(defun is (got expected &rest args)
  (or (test got expected args)
      (format *test-result-output* "~&#   got: ~S~%#   expected: ~S~%" got expected)))

@export
@doc "tests (not (equal got expected))"
(defun isnt (got expected &rest args)
  (or (test got expected args :notp t)
      (format *test-result-output* "~&#   got: ~S~%#   not expected: ~S~%" got expected)))

@export
@doc "just outputs the message"
(defun diag (desc)
  (format *test-result-output* "~&# ~a~%" desc))

(defun gensymp (val)
  (and (symbolp val)
       (string= (subseq (symbol-name val) 0 (length *gensym-prefix*)) *gensym-prefix*)))

(defmethod gensym-tree-equal (x y)
  (if (and (gensymp y) (symbolp x))
      (if (assoc y *gensym-alist*)
          (eq x (cdr (assoc y *gensym-alist*)))
          (unless (rassoc x *gensym-alist*)
            (setf *gensym-alist* `((,y . ,x) ,@*gensym-alist*))
            t))
      (equal x y)))

(defmethod gensym-tree-equal ((x cons) (y cons))
  (loop for a in x for b in y
        always (gensym-tree-equal a b)))

@export
@doc "tests (equalp (macroexpand-1 'got) expected)"
(defmacro is-expand (got expected &optional desc)
  (let ((expanded (gensym)))
    `(let ((,expanded (macroexpand-1 ',got))
           *gensym-alist*)
       (or (test ,expanded ',expected ,desc :test #'gensym-tree-equal)
           (format *test-result-output*
                   "~&#   got: ~S~%#   expanded: ~S~%#   expected: ~S~%"
                   ',got ,expanded ',expected)))))

@export
@doc "tests the output result of `got'"
(defmacro is-print (got expected &optional desc)
  (let ((res (gensym)))
    `(let ((,res (with-output-to-string (*standard-output*) ,got)))
       (test ,res ,expected ,desc))))


@export
@doc "tests the error thrown by `got'"
(defmacro is-error (form condition &optional desc)
  (let ((err (gensym)))
    `(let ((,err (handler-case ,form
                   (condition (error) error))))
       (or (test (typep ,err ,(if (and (listp condition) (eq 'quote (car condition)))
                                  condition
                                  `(quote ,condition)))
                 t ,desc)
           (format *test-result-output*
                   "~&#   got: ~S~%#   expected error: ~S~%"
                   ,err ',condition)))))

@export
@doc "tests the type of the result of `got'"
(defun is-type (got expected-type &optional desc)
  (or (test (typep got expected-type) t desc)
      (format *test-result-output*
              "~&#   got: ~S~%#   expected type: ~S~%"
              got expected-type)))

@export
@doc "tests if the regex matches the output result of `got'"
(defun like (got regex &optional desc)
  (or (test (numberp (ppcre:scan regex got)) t desc)
      (format *test-result-output*
              "~&#   got: ~S~%#   like: ~S~% ~S~%"
              got regex (ppcre:scan regex got))))

@export
@doc "skip the next n tests"
(defun skip (how-many why &rest args)
  (with-package-symbols (*counter*)
    (dotimes (i (or how-many 1))
      (incf *counter*)
      (format *test-result-output*
              "~&ok ~A # skip~:[~;~:* ~A~]~%"
              *counter* (apply #'format nil why args)))))

@export
@doc "it always passes"
(defun pass (desc &rest args)
  (test t t (apply #'format nil desc args)))

@export
@doc "it always fails"
(defun fail (desc &rest args)
  (test t nil (apply #'format nil desc args)))

(defun find-test (name)
  (assoc name *tests*))

@export
@doc "List all tests defined in a package specified in the first argument."
(defun find-tests-of-package (pkg &aux (package (find-package pkg)))
  (reverse
   (remove-if-not
    #'(lambda (test)
        (eq (symbol-package (car test))
            package))
    *tests*)))

@export
@doc "define a named group of tests in this package.
Each test is defined in the `body' form, which is an implicit progn."
(defmacro deftest (name &body test-forms)
  (let ((test (gensym))
        (test-fn (gensym)))
    `(let ((,test (find-test ',name))
           (,test-fn
            (lambda ()
              (with-package-symbols (*plan* *counter* *failed*)
                (let (successp)
                  (progv
                      `(*indent-level*
                        ,(intern (string :*plan*) *package*)
                        ,(intern (string :*counter*) *package*)
                        ,(intern (string :*failed*) *package*))
                      `(,(1+ *indent-level*) nil 0 0)
                    ,@test-forms
                    (setf successp (finalize)))
                  (if successp
                      (pass (string ',name))
                      (fail (string ',name))))))))
       (if ,test
           (rplacd ,test ,test-fn)
           (push (cons ',name
                       ,test-fn)
                 *tests*)))))


@export
@doc "Run a test defined within the current package."
(defun run-test (name)
  (let ((test (find-test name)))
    (if test
        (funcall (cdr test))
        (error "Not found test: ~a" (car test)))))

@export
@doc "Run all tests in the `package'."
(defun run-test-package (package)
  @type (or string symbol package) package
  (plan (handler-case (symbol-value (intern (string :*plan*) package))
          (unbound-variable () :unspecified)))
  (loop for (name . nil) in (find-tests-of-package package)
        do (run-test name))
  (finalize))

@export
@doc "DEPRECATED!"
(defun run-test-all ()
  (loop for (nil . body) in (reverse *tests*)
        do (funcall body))
  (finalize))

@export
@doc "remove the test named `name' in the current package."
(defun remove-test (name)
  (setf *tests* (delete name *tests* :key #'car :test #'string=)))

@export
@doc "remove all tests in the current package."
(defun remove-test-all ()
  (setf *tests* nil))
