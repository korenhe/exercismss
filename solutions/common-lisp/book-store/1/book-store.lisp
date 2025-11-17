(defpackage :book-store
  (:use :cl)
  (:export :calculate-price))

(in-package :book-store)

;; (defun permutations (list)
;;   (if (null list)
;;       (list nil)
;;       (mapcan (lambda (x)
;;                 (mapcar (lambda (perm) (cons x perm))
;;                         (permutations (remove x list :count 1))))
;;               list)))

(defun combinations (list n)
  (cond
    ((zerop n) (list nil))
    ((null list) nil)
    (t
     (let ((head (first list))
           (tail (rest list)))
       (append
        ;; choose head
        (mapcar (lambda (c) (cons head c))
                (combinations tail (1- n)))
        ;; skip head
        (combinations tail n))))))

(defun remove-ll-from-list (ll rawlist)
  (loop for l in ll
        do (setf rawlist (delete l rawlist :count 1))
        finally(return rawlist)
        )
  )

(defun price-of (ll)
  (case (length ll)
    (1 800)
    (2 (floor (* 1600 0.95)))
    (3 (floor (* 2400 0.90)))
    (4 (floor (* 3200 0.80)))
    (5 (floor (* 4000 0.75)))
    )
  )

(defparameter *memo* (make-hash-table :test 'equal))

(defun htab-key (rawlist)
  (sort rawlist #'<))

(declaim (ftype function calculate-price))

(defun lowest-price-wp (rawlist)
  (let ((key (htab-key rawlist)))
    (or (gethash key *memo*)
        (setf (gethash key *memo*)
              (calculate-price rawlist)))))

(defun calculate-price (basket)
  ;; convert basket seq to htab
  (let* (
         (htab (make-hash-table))
         (klist '())
         )
    (dolist (n basket) (incf (gethash n htab 0)))
    (maphash (lambda (k v) (push k klist)) htab)
    (if (zerop (length klist)) 0
        (loop
          for n from 1 to (length klist)
          minimize (loop
                     for ll in (combinations klist n)
                     minimize (+ (price-of ll)
                                 (lowest-price-wp
                                  (remove-ll-from-list ll (copy-list basket)))))
          )))
  )
