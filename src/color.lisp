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

(defmacro with-gray (stream &body body)
  `(progn
     (format ,stream (cl-ansi-text::generate-color-string 90))
     (unwind-protect (progn ,@body)
       (format ,stream (cl-ansi-text::generate-color-string 0)))))

(defmacro with-color-if-available ((color &rest args) &body body)
  `(if (color-available-p)
       (if (or (eq ,color :gray)
               (eq ,color cl-colors:+gray+))
           (with-gray ,(or (getf args :stream) t) ,@body)
           (with-color (,color ,@args) ,@body))
       (progn ,@body)))
