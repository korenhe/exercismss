(defpackage :etl
  (:use :cl)
  (:export :transform))

(in-package :etl)

(defun transform (data)
  "Transforms hash values into keys with their keys as their values."
  (loop
	with rtable = (make-hash-table :test 'eq)
	for point being the hash-key of data
	for keys being the hash-value of data
	do (loop for xkey in keys
			 for key = (char-downcase xkey)
			 do (setf (gethash key rtable) point)
			 )
	finally (return rtable)
	)
  )
