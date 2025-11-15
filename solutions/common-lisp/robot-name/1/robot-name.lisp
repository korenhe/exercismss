(defpackage :robot-name
  (:use :cl)
  (:export :build-robot :robot-name :reset-name))

(in-package :robot-name)

(defstruct robot
  sid
  )

(defparameter *robotlist* (make-hash-table))

(defun generate-rand-name ()
  (coerce (append
   (loop repeat 2 collect (code-char (+ (random 26) (char-code #\A))))
   (loop repeat 3 collect (code-char (+ (random 10) (char-code #\0)))))
          'string))

(defun build-robot()
  (make-robot :sid (loop
                     for name = (generate-rand-name)
                     while (gethash name *robotlist*)
                     finally (return name)
                     )))

(defun robot-name (rob)
  (robot-sid rob))

(defun reset-name (rob)
  (setf (robot-sid rob) (loop
                          for name = (generate-rand-name)
                          while (gethash name *robotlist*)
                          finally (return name)
                          )))
