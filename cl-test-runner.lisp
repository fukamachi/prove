
(in-package :cl-test-more)
(annot:enable-annot-syntax)

(defclass test ()
  ((name :type symbol
		 :initarg :name
		 :accessor name-of)
   (package :type package
			:initarg :package
			:accessor package-of)
   (body :type function
		 :initarg :body
		 :accessor body-of)))

(defmethod print-object ((test test) s)
  (print-unreadable-object (test s :type t)
	(format s "~a in ~a"
			(name-of test)
			(package-of test))))

;; (defmethod initialize-instance :after ((test test) &key)
;;   (unless (slot-boundp test 'package)
;; 	(setf (package-of test) 
;; 		  (symbol-package (name-of test)))))

@export
(defun find-test (name)
  (find-if (lambda (test)
			 (eq name (name-of test)))
		   *tests*))

@export
@doc "List all tests defined in a package specified in the first argument."
(defun find-tests-of-package (pkg &aux (package (find-package pkg)))
  (nreverse
   (remove-if-not
    #'(lambda (test)
        (eq (package-of test)
            package))
    *tests*)))

@export
@doc "Define a named group of tests.
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
					(catch 'deftest-error
					  (handler-bind
						  ((error
							(lambda (e)
							  (format
							   *test-result-output*
							   "~&# Tests terminated out of the test clauses:~%~&### ~a"
							   e)
							  (setf successp nil)
							  (throw 'deftest-error e))))
						,@test-forms
						(setf successp (finalize)))))
				  (if successp
					  (pass (string ',name))
					  (fail (string ',name))))))))
	   (if ,test
		   (setf (body-of ,test) ,test-fn)
		   (push (make-instance 'test
								:name ',name
								:package *package*
								:body ,test-fn)
				 *tests*))
	   ,test)))

;; obsolete: no longer working
;; (defun run-test (name)
;;   (let ((test (find-test name)))
;;     (if test
;;         (funcall (body-of test))
;;         (error "Not found test: ~a" (car test)))))

@export
@doc "run the test"
(defun run-test (test)
  @type test test
  (funcall (body-of test)))

@export
@doc "Run all tests in the `package'."
(defun run-test-package (package)
  @type (or string symbol package) package
  (plan (handler-case (symbol-value (intern (string :*plan*) package))
          (unbound-variable () :unspecified)))
  (format *test-result-output* "doing test in package ~A" package)
  (loop for test in (find-tests-of-package package)
        do (run-test test))
  (finalize))


@export
@doc "Walk down through the package hierarchy under `package' and
 recursively run all the tests in those packages.
 e.g. it considers `:pkg.a' as a module of `:pkg', and `:pkg.a.b' 
as a module of `pkg.a' and so on."
(defun run-test-recursively (package)
  @type (or string symbol package) package
  (let* ((package (if (packagep package) package
					  (find-package package)))
		 (name (package-name package))
		 (children (remove-if-not 
					(lambda (pkg)
					  (scan (concatenate 'string
										 "^" name "\\.[^.]*$")
							(package-name pkg)))
					(list-all-packages))))
	(run-test-package package)
	(when children
	  (prog2
		  (incf *indent-level*)
		  (mapcar #'run-test-recursively children)
		(decf *indent-level*)))))

;; @export
;; @doc "DEPRECATED!"
;; (defun run-test-all ()
;;   (loop for (nil . body) in (reverse *tests*)
;;         do (funcall body))
;;   (finalize))

@export
@doc "remove the test named `name' in the current package."
(defun remove-test (name)
  (setf *tests*
		(delete-if (lambda (test) (eq name (name-of test)))
				   *tests*)))

@export
@doc "remove all tests in `package'"
(defun remove-test-of-package (package)
  (let ((package (if (packagep package) package
					 (find-package package))))
	(setf *tests*
		  (delete-if (lambda (test) (eq package (package-of test)))
					 *tests*))))

@export
@doc "remove all tests in the current package."
(defun remove-test-all ()
  (setf *tests* nil))
