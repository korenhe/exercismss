(defpackage :resistor-color
  (:use :cl)
  (:export :color-code
           :colors))

(in-package :resistor-color)

(defparameter *colormap*
  '(
	("black" . 0)
	("brown" . 1)
	("red" . 2)
	("orange" . 3)
	("yellow" . 4)
	("green" . 5)
	("blue" . 6)
	("violet" . 7)
	("grey" . 8)
	("white" . 9)
	))

(defun color-code (color)
  (cdr (assoc color *colormap* :test #'string=))
  )

(defun colors ()
  (loop for pair in *colormap*
		collect (car pair)
		)
  )
