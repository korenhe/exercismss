(defpackage :matrix
  (:use :cl)
  (:export :row
           :column))

(in-package :matrix)

(defun row (input-matrix index)
  (let* ((mm (with-input-from-string (in input-matrix)
               (loop for line = (read-line in nil)
                     while line
                     collect (read-from-string (format nil "(~a)" line))))))
    (nth (1- index) mm)
    ))

(defun column (input-matrix index)
  (let* ((mm (with-input-from-string (in input-matrix)
               (loop for line = (read-line in nil)
                     while line
                     collect (read-from-string (format nil "(~a)" line))))))
    (mapcar (lambda (x) (nth (1- index) x)) mm)
    )
  )
