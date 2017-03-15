(in-package :cl-user)
(defpackage prove.t.utils
  (:use :cl)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :prove
                :like
                :subtest
                :is)
  (:export :test-assertion))
(in-package :prove.t.utils)


(defun empty-line-p (line)
  "Checks if line of text is empty."
  (equal line ""))


(defun get-indentation (line)
  "Returns numbers of leading spaces for the line."
  (loop
     :for char :across line
     :for num-spaces :upfrom 0
     :when (not (equal char #\Space))
     :do (return num-spaces)))


(defun left-remove-if (items predicate)
  "Returns list skipping leftmost items
 which match a predicate."
  (do ()
      ((not (funcall predicate (car items))) items)
    (setf items (cdr items))))

(defun right-remove-if (items predicate)
  "Returns a new list, without rightmost items
which match a predicate."
  (labels ((recur (items)
             (destructuring-bind (head . tail) items
               (if tail
                   (let ((tail (recur tail)))
                     (if tail
                         (cons head tail)
                         (unless (funcall predicate head)
                           (list head))))
                   (if (funcall predicate head)
                       nil
                       (list head))))))
    (recur items)))


(defun deindent (text)
  "Removes empty new lines at the begining and at the end of the text,
and removes common number of whitespaces from rest of the lines."
  (let* ((all-lines (split-sequence
                     #\Newline
                     text))
         ;; remove empty lines at beginning
         (left-trimmed (left-remove-if
                        all-lines
                        #'empty-line-p))
         ;; and at the end
         (lines (right-remove-if
                 left-trimmed
                 #'empty-line-p))
         
         ;; calculate common indentation
         (min-indent (apply #'min (mapcar #'get-indentation lines)))
         
         ;; remove common indentation from lines
         (new-lines (loop :for line :in lines
                       :collect (subseq line min-indent))))
    
    ;; now join lines together and separate them with new-lines
    (values (format nil "~{~a~^~%~}" new-lines)
            min-indent)))


(defmacro test-assertion (title body expected
                          &aux (method (if (search ".*" expected)
                                           'like
                                           'is)))
  "Tests that assertion result in prove's output
matches given regular expression.

Body evaluated and it's result is matched agains expected string,
using prove:like. Dangling spaces and newlines are trimmed from
the result before trying to match."
  
  (with-gensyms (result trimmed-result deindented-expected)
    `(subtest ,title
       (let* ((,result
               ;; All output during the test, should be captured
               ;; to test against give regex
               (with-output-to-string
                   (prove.output:*test-result-output*)

                 (let ( ;; Colors whould be turned off to
                       ;; prevent Prove's reporter return
                       ;; string with terminal sequences.
                       ;; This way it will be easier to compare
                       ;; results with usual strings
                       (prove.color:*enable-colors* nil)
                       ;; We need to overide current suite, to prevent
                       ;; tested assert-that macro from modifying real testsuite.
                       ;; Otherwise it can increment failed or success tests count
                       ;; and prove will output wrong data.
                       (prove.suite:*suite* (make-instance 'prove.suite:suite))
                       (prove.reporter::*debug-indentation* nil))
                   ,body)))

              (,trimmed-result (string-trim '(#\Space #\Newline)
                                            (deindent ,result)))
              (,deindented-expected (deindent ,expected)))
         (,method ,trimmed-result
                  ,deindented-expected)))))
