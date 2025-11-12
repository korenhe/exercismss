(defpackage :proverb
  (:use :cl)
  (:export :recite))

(in-package :proverb)

(defun recite (strings)
  (if (null strings)
	  ""
  (apply #'concatenate 'string (
								loop
								 for ff in strings
								 for ss in (subseq strings 1)
								 collect (format nil "For want of a ~A the ~A was lost.~%" ff ss) into result
								 finally (return (append result (list
																  (format nil "And all for the want of a ~A." (first strings))
																  )))
		)))
  )
