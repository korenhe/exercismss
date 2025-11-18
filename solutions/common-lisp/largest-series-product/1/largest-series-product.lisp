(defpackage :largest-series-product
  (:use :cl)
  (:export :largest-product))

(in-package :largest-series-product)

(defun largest-product (digits span)
  (let ((len (length digits)))
    (if (and (and (> span 0) (> len 0)) (>= len span))
        (loop
          for i from 0
          repeat (1+ (- len span))
          for subss = (subseq digits i (+ i span))
          for sublt = (coerce subss 'list)
          maximize (reduce (lambda (acc x)
                             (if (not (digit-char-p x)) (return-from largest-product nil))
                             (* acc (- (char-code x) (char-code #\0)))
                             )
                           sublt :initial-value 1)))))
