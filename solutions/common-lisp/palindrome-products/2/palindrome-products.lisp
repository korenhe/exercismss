(defpackage :palindrome-products
  (:use :cl)
  (:export :smallest
           :largest))

(in-package :palindrome-products)

(defun get-palindrome-products (min-factor max-factor &optional (htab (make-hash-table :test #'equal)))
  (sort (remove-duplicates
                        (
                         loop
                          for a from min-factor to max-factor
                          append (loop for b from a to max-factor
                                       for product = (* a b)
                                       for xstr = (format nil "~A" product)
                                       when (string= xstr (reverse xstr))
                                         collect product
                                         and
                                           do (setf (gethash product htab)
                                                    (push (list a b) (gethash product htab))))
                         ))
        #'<
        ))

(defun smallest (min-factor max-factor)
  (let* ((htab (make-hash-table))
         (factlist (get-palindrome-products min-factor max-factor htab))
         )
    (values (first factlist) (reverse (gethash (first factlist) htab)))
    ))

(defun largest (min-factor max-factor)
  (let* ((htab (make-hash-table :test #'equal))
         (factlist (get-palindrome-products min-factor max-factor htab))
         )
    (values (car (last factlist)) (reverse (gethash (car (last factlist)) htab)))))
