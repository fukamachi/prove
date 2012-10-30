
(in-package :cl-test-more)

(defmacro define-test-handler (name args &rest body)
  `(defmacro ,name ,args
	 `(catch 'test-error
		(handler-bind ((error
						(lambda (e)
						  
						  (test t nil (format nil ;*test-result-output*
								  "Thrown error: ~a" e))
						  (throw 'test-error e))))
		  ,,@body))))

@export
@doc "just outputs the message"
(define-test-handler diag (desc-or-obj &rest args)
  `(if (stringp ,desc-or-obj)
	  (format *test-result-output* 
			  (concatenate 'string
						   "~&# " ,desc-or-obj "~%")
			  ,@args)
	  (format *test-result-output* 
			  "~&# ~a~%" ,desc-or-obj)))



@export
@doc "tests if the first argument is non-nil"
(define-test-handler ok (test &optional desc)
  `(test (not (null ,test)) t ,desc))

@export
@doc "tests if the first argument is nil"
(define-test-handler ng (test &optional desc)
  `(test (not (null ,test)) t ,desc :notp t))

@export
@doc "tests (equal got expected)"
(define-test-handler is (got expected &rest args)
  `(or (test ,got ,expected ,args)
	   (format *test-result-output*
			   "~&#   got: ~S~%#   expected: ~S~%"
			   ,got ,expected)))

@export
@doc "tests (not (equal got expected))"
(define-test-handler isnt (got expected &rest args)
  `(or (test ,got ,expected ,args :notp t)
      (format *test-result-output*
			  "~&#   got: ~S~%#   not expected: ~S~%"
			  ,got ,expected)))

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

;; @export
;; @doc "tests (equalp (macroexpand-1 'got) expected)"
;; (defmacro is-expand (got expected &optional desc)
;;   (let ((expanded (gensym)))
;;     `(let ((,expanded (macroexpand-1 ',got))
;;            *gensym-alist*)
;;        (or (test ,expanded ',expected ,desc :test #'gensym-tree-equal)
;;            (format *test-result-output*
;;                    "~&#   got: ~S~%#   expanded: ~S~%#   expected: ~S~%"
;;                    ',got ,expanded ',expected)))))

;; @export
;; @doc "tests the output result of `got'"
;; (defmacro is-print (got expected &optional desc)
;;   (let ((res (gensym)))
;;     `(let ((,res (with-output-to-string (*standard-output*) ,got)))
;;        (test ,res ,expected ,desc))))


;; @export
;; @doc "tests the error thrown by `got'"
;; (defmacro is-error (form condition &optional desc)
;;   (let ((err (gensym)))
;;     `(let ((,err (handler-case ,form
;;                    (condition (error) error))))
;;        (or (test (typep ,err ,(if (and (listp condition) (eq 'quote (car condition)))
;;                                   condition
;;                                   `(quote ,condition)))
;;                  t ,desc)
;;            (format *test-result-output*
;;                    "~&#   got: ~S~%#   expected error: ~S~%"
;;                    ,err ',condition)))))

@export
@doc "tests the type of the result of `got'"
(define-test-handler is-type (got expected-type &optional desc)
  `(or (test (typep ,got ,expected-type) t ,desc)
      (format *test-result-output*
              "~&#   got: ~S~%#   expected type: ~S~%"
              ,got ,expected-type)))

@export
@doc "tests if the regex matches the output result of `got'"
(define-test-handler like (got regex &optional desc)
  `(or (test (numberp (ppcre:scan ,regex ,got)) t ,desc)
      (format *test-result-output*
              "~&#   got: ~S~%#   like: ~S~% ~S~%"
              ,got ,regex (ppcre:scan ,regex ,got))))

@export
@doc "skip the next n tests"
(define-test-handler skip (how-many why &rest args)
  `(with-package-symbols (*counter*)
    (dotimes (i (or ,how-many 1))
      (incf *counter*)
      (format *test-result-output*
              "~&ok ~A # skip~:[~;~:* ~A~]~%"
              *counter* (apply #'format nil ,why ,args)))))

@export
@doc "it always passes"
(define-test-handler pass (desc &rest args)
  `(test t t (apply #'format nil ,desc ,args)))

@export
@doc "it always fails"
(define-test-handler fail (desc &rest args)
  `(test t nil (apply #'format nil ,desc ,args)))


@export
@doc "Terminates the current test (defined by deftest) and fails the test.
Then go to the next test. (needs improvement)"
(defmacro terminate (desc &rest args)
  `(error ,desc ,@args))


