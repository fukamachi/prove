(in-package :cl-user)
(defpackage prove.color
  (:use :cl)
  #-abcl
  (:import-from :cl-ansi-text
                :generate-color-string)
  (:export :*enable-colors*
           :with-color))
(in-package :prove.color)

;; Notice for ABCL users
;; Colorizing is not supported on ABCL for now
;; because cl-ansi-text cannot be loaded due to a strange behaviour of cl-colors on ABCL.
;; See https://github.com/tpapp/cl-colors/issues/6 for the detail of the issue.

(defvar *enable-colors*
  #+abcl nil
  #-abcl (not (equal (asdf::getenv "EMACS") "t"))
  "Flag whether colorize a test report. The default is T except on Emacs (SLIME).")

#-abcl
(defmacro with-gray (stream &body body)
  `(progn
     (format ,stream (cl-ansi-text::generate-color-string 90))
     (unwind-protect (progn ,@body)
       (format ,stream (cl-ansi-text::generate-color-string 0)))))

#-abcl
(defmacro with-color ((color &rest args) &body body)
  (cond
    ((or (eq color :gray)
         (eq color :grey))
     `(if *enable-colors*
          (with-gray ,(or (getf args :stream) t) ,@body)
          (progn ,@body)))
    (T `(if *enable-colors*
            (if (or (eq ,color :gray)
                    (eq ,color :grey)
                    (eq ,color cl-colors:+gray+)
                    (eq ,color cl-colors:+grey+))
                (with-gray ,(or (getf args :stream) t) ,@body)
                (cl-ansi-text:with-color (,color ,@args) ,@body))
            (progn ,@body)))))

#+abcl
(defmacro with-color ((color &rest args) &body body)
  (declare (ignore color args))
  `(progn ,@body))
