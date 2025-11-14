(defpackage :twelve-days
  (:use :cl)
  (:export :recite))

(in-package :twelve-days)

(defconstant +orders+ '("first" "second" "third"
						 "fourth" "fifth" "sixth"
						 "seventh" "eighth" "ninth"
						 "tenth" "eleventh" "twelfth"
						 ))
(defconstant +items+ '(
						"a Partridge"
						"two Turtle Doves"
						"three French Hens"
						"four Calling Birds"
						"five Gold Rings"
						"six Geese-a-Laying"
						"seven Swans-a-Swimming"
						"eight Maids-a-Milking"
						"nine Ladies Dancing"
						"ten Lords-a-Leaping"
						"eleven Pipers Piping"
						"twelve Drummers Drumming"
						))

(defun recite (&optional (begin 1 begin-supplied-p) (end 12 end-supplied-p))
  "Returns a string of the requested verses for the 12 Days of Christmas."
  ;; If only BEGIN was supplied, make END = BEGIN
  (when (and begin-supplied-p (not end-supplied-p))
    (setf end begin))

  (format nil "~{~A~^~%~}" (loop
	with accitems = '()
	repeat 12
	for i from 0
	do (setf accitems (push (nth i +items+) accitems))
	when (and (>= (1+ i) begin) (< i end))
	  if (> (length accitems) 1)
		collect (format nil "On the ~A day of Christmas my true love gave to me: ~{~A, ~}~A in a Pear Tree." (nth i +orders+) (butlast accitems) (format nil "and ~A" (car (last accitems))))
	else collect (format nil "On the ~A day of Christmas my true love gave to me: ~{~A~^, ~} in a Pear Tree." (nth i +orders+) accitems)
	))
  )
