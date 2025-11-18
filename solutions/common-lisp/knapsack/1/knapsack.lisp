(defpackage :knapsack
  (:use :cl)
  (:export :maximum-value))

(in-package :knapsack)

(defparameter *memo* (make-hash-table :test #'equal))

(defun maximum-value (maximum-weight items)
  (let ((res (gethash (cons maximum-weight items) *memo*)))
    (if res (return-from maximum-value res))
    )

  (let* ((res
           (reduce
            (lambda (acc pair)
              (max
               acc
               (if (>= maximum-weight (cdr (assoc :weight pair)))
                   (+ (cdr (assoc :value pair))
                      (maximum-value (- maximum-weight (cdr (assoc :weight pair))) (remove pair items :test #'eql)
                                     ))
                   acc
                   )
               )) items :initial-value 0)))
    (setf (gethash (cons maximum-weight items) *memo*) res)
    )
  )
