(defpackage :luhn
  (:use :cl)
  (:export :validp))

(in-package :luhn)

(defun validp (input)
  (let* (
         (lnum (coerce input 'list))
         (vnum (remove-if #'null
                          (mapcar (lambda (x) (cond
                                                ((digit-char-p x) (- (char-code x) (char-code #\0)))
                                                ((char= x #\ ) nil)
                                                (t (return-from validp nil))
                                                )) lnum)))
         (rnum (reverse vnum))
         (result     (loop for n in rnum
                           for i from 0
                           if (/= 0 (mod i 2))
                             sum (let* ((x (* 2 n)))
                                   (if (> x 9) (- x 9) x)
                                   )
                           else
                             sum n
                           ))
         )
    (if (< (length rnum) 2) (return-from validp nil))
    (if (= 0 (mod result 10)) t)
    )
  )
