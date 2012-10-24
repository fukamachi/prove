
(in-package :cl-test-more)

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
       (string= (subseq (symbol-name val) 0 (length *gensym-prefix*))
				*gensym-prefix*)))

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
