(defpackage :resistor-color-trio
  (:use :cl)
  (:export :label))

(in-package :resistor-color-trio)

(defun color-to-digit (color)
  (position color #("black" "brown" "red"
                    "orange" "yellow" "green"
                    "blue" "violet" "grey"
                    "white"
                    ) :test #'equal)
  )

(defun to-ohm (value)
  (car (last
   (loop for i from 0 to 10
         for ii = (expt 1000 i)
         for unit in '("ohms" "kiloohms" "megaohms" "gigaohms")
         while (floor value ii)
         if (zerop (mod value ii))
           collect (format nil "~A ~A" (floor value ii) unit)
         until (zerop value)
         ))))

(defun label (colors)
  (to-ohm (loop for color in colors
        for i from 0
        repeat 3
        if (zerop i)
          sum (* 10 (color-to-digit color)) into result
        else
          if (= i 1)
               sum (color-to-digit color) into result
        else
          do (setf result (* result (expt 10 (color-to-digit color))))
        finally (return result)
        ))
  )
