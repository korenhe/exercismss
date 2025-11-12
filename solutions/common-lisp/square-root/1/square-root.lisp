(defpackage :square-root
  (:use :cl)
  (:export :square-root))

(in-package :square-root)

(defun square-inner (target begin end)
  (if (<= begin end)
	  (let* (
			 (mid (floor (+ begin end) 2))
			 (mid2 (* mid mid))
			 (mid2p (* (1+ mid) (1+ mid)))
			 )
		(cond
		  ((or (= mid2 target) (and (> mid2p target) (< mid2 target))) mid)
		  ((< mid2 target) (square-inner target (1+ mid) end))
		  ((> mid2 target) (square-inner target begin mid))
		  )
		)
  ))


(defun square-root (radicand)
  (square-inner radicand 1 radicand)
  )
