(defpackage :resistor-color-duo
  (:use :cl)
  (:export :value))

(in-package :resistor-color-duo)

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

(defun value (colors)
  (loop for color in colors
		repeat 2
		for i in '(10 1)
		sum (* i (color-code color))
		)
  )
