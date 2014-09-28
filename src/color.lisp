(in-package :cl-user)
(defpackage cl-test-more.color
  (:use :cl)
  (:import-from :cl-ansi-text
                :with-color)
  (:export :*force-enable-colorize*
           :with-color-if-available))
(in-package :cl-test-more.color)

(defvar *force-enable-colorize* nil)

(defun color-available-p ()
  (or (not (equal (asdf::getenv "EMACS") "t"))
      *force-enable-colorize*))

(defmacro with-color-if-available ((color &rest args) &body body)
  `(if (color-available-p)
       (with-color (,color ,@args) ,@body)
       (progn ,@body)))
