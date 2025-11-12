(defpackage :bottle-song
  (:use :common-lisp)
  (:export :recite))

(in-package :bottle-song)


(defun recite (start-bottles take-down)
  "Returns the song verses from START-BOTTLES down to (- START-BOTTLES TAKE-DOWN)."
  (let* (
		 (nums '("no"
				 "One"
				 "Two"
				 "Three"
				 "Four"
				 "Five"
				 "Six"
				 "Seven"
				 "Eight"
				 "Nine"
				 "Ten"))
		 (tuples (loop for i from start-bottles above 0
					  repeat take-down
					  for index1 = i
					  for index2 = (1- i)
					  collect (list (nth index1 nums) index1 index2 (string-downcase (nth index2 nums)))
					  )
				 ))
	(loop for (a b c d) in tuples
		  for i from 1
		  for total = (length tuples)

		  collect (format nil "~A green bottle~*~:P hanging on the wall," a b)
		  collect (format nil "~A green bottle~*~:P hanging on the wall," a b)
		  collect (format nil "And if one green bottle should accidentally fall,")
		  collect (format nil "There'll be ~A green bottle~*~:P hanging on the wall." d c)

		  if (/= i total)
			collect ""
		  )
	))
