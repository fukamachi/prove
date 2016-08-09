(in-package :cl-user)
(defpackage prove.color
  (:use :cl)
  (:import-from :cl-ansi-text
                :generate-color-string)
  (:import-from :cl-colors
                :+gray+
                :+grey+)
  (:export :*enable-colors*
           :with-color))
(in-package :prove.color)

(defvar *enable-colors*
  (not (equal (uiop:getenv "EMACS") "t"))
  "Flag whether colorize a test report. The default is T except on Emacs (SLIME).")

(defmacro with-gray (stream &body body)
  `(progn
     (format ,stream (cl-ansi-text::generate-color-string 90))
     (unwind-protect (progn ,@body)
       (format ,stream (cl-ansi-text::generate-color-string 0)))))

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
