(defpackage :crypto-square
  (:use :cl)
  (:export :encipher))
(in-package :crypto-square)

(defun getrc (len)
  (let* (
         (r (floor (sqrt len)))
         (c1 r)
         (c2 (1+ r)))
    (cond
      ((>= (* r c1) len) (cons r c1))
      ((>= (* r c2) len) (cons r c2))
      (t (cons c2 c2))
      )))

(defun encipher (plaintext)
  (let* (
         (normalized (string-downcase (remove-if-not
                                       (lambda (x)
                                         (or (alpha-char-p x) (digit-char-p x))) plaintext)))
         (rc (getrc (length normalized)))
         (r (car rc))
         (c (cdr rc))
         (matrix (make-array (list r c))))
    (loop
      for i from 0 below r
      do (loop
           for j from 0 below c
           for index = (+ (* i c) j)
           for tx = (if (>= index (length normalized)) #\ (aref normalized index))
           do (setf (aref matrix i j) tx)
           ))

    (coerce (loop
      for j from 0 below c
      append (loop
           for i from 0 below r
           collect (aref matrix i j)
           )
      when (< j (1- c))
        append (list #\ )
      ) 'string)))
