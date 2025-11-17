(defpackage :palindrome-products
  (:use :cl)
  (:export :smallest
           :largest))

(in-package :palindrome-products)

(defun smallest (min-factor max-factor)
  (let* ((htab (make-hash-table))
         (ssproduct nil)
         )
    (
     loop
     for a from min-factor to max-factor
     append (loop for b from a to max-factor
                  for product = (* a b)
                  for xstr = (format nil "~A" product)
                  when (and (or (null ssproduct) (<= product ssproduct))
                            (string= xstr (reverse xstr)))
                    do (setf ssproduct product) and
                    collect product
                    and
                      do (setf (gethash product htab)
                               (push (list a b) (gethash product htab))))
     )
    (values ssproduct (reverse (gethash ssproduct htab)))
    ))

(defun largest (min-factor max-factor)
  (let* ((htab (make-hash-table))
         (ssproduct nil)
         )
    (
     loop
     for a from min-factor to max-factor
     append (loop for b from a to max-factor
                  for product = (* a b)
                  for xstr = (format nil "~A" product)
                  when (and (or (null ssproduct) (>= product ssproduct))
                            (string= xstr (reverse xstr)))
                    do (setf ssproduct product) and
                    collect product
                    and
                      do (setf (gethash product htab)
                               (push (list a b) (gethash product htab))))
     )
    (values ssproduct (reverse (gethash ssproduct htab)))
    ))
