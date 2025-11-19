(defpackage :pythagorean-triplet
  (:use :cl)
  (:export :triplets-with-sum))

(in-package :pythagorean-triplet)

(defun triplets-with-sum (n)
  (loop for a from 1 to (floor n 3)
        for aa = (* a a)
        append (loop for b from a to n
                     for c = (- n (+ a b))
                     while (< b c)
                     if (> (+ a b) c)
                       if (= (+ aa (* b b)) (* c c))
                         collect (list a b c)
                     )
        )
  )
