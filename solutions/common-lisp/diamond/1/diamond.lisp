(defpackage :diamond
  (:use :cl)
  (:export :rows))

(in-package :diamond)

(defun pp-line (i h)
  (format nil "~A~A~A~A~A"
          (coerce (loop for j from i below h
                        collect #\Space) 'string)
          (code-char (+ i (char-code #\A)))
          (coerce (loop for j from 0 to (- (* i 2) 2)
                        collect #\Space) 'string)
          (if (> i 0)
              (code-char (+ i (char-code #\A))) "")
          (coerce (loop for j from i below h
                        collect #\Space) 'string)
          ))

(defun rows (letter)
  (let* ((h (- (char-code letter) (char-code #\A)))
         (toph (1+ h))
         )
    (append (loop for i from 0 below toph
                  collect (pp-line i h)
                  )

            (loop for i from (- toph 2) downto 0
                  collect (pp-line i h)
                  ))
    ))
