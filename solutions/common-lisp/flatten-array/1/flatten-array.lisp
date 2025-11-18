(defpackage :flatten-array
  (:use :cl)
  (:export :flatten))

(in-package :flatten-array)

(defun flatten (nested)
  (loop
    for el in nested
    if (listp el)
      append (flatten el)
    else
      append (list el)
    )
  )
