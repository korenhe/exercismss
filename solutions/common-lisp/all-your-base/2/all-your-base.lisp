(defpackage :all-your-base
  (:use :cl)
  (:export :rebase))

(in-package :all-your-base)

(defun to-decimal (list-digits in-base)
  (loop for dig in (reverse list-digits)
		for i from 0
		if (or (>= dig in-base) (< dig 0))
		   do (return-from to-decimal nil)
		sum (* dig (expt in-base i)))
  )

(defun decimal-to (dec out-base)
  (if dec
	  (loop with current = dec
			while (> current 0)
			collect (mod current out-base) into result
			do (setf current (floor current out-base))
			finally (progn
					  (if (zerop (length result)) (push 0 result))
					  (return (nreverse result))))))

(defun rebase (list-digits in-base out-base)
  (if (or (< in-base 2) (< out-base 2)) nil
	  (decimal-to (to-decimal list-digits in-base) out-base)))
