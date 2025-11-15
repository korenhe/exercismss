(defpackage :nucleotide-count
  (:use :cl)
  (:export :nucleotide-counts))

(in-package :nucleotide-count)

(defun nucleotide-counts (strand)
  (let ((table (make-hash-table)))
    ;; initialize counts
    (dolist (n '(#\A #\C #\G #\T))
      (setf (gethash n table) 0))

    ;; process strand
    (loop for ch across strand do
          (if (find ch '(#\A #\C #\G #\T))
              (incf (gethash ch table))
              (return-from nucleotide-counts nil)))

    ;; return as alist
    (let (res)
      (maphash (lambda (k v)
                 (push (cons k v) res))
               table)
      (sort res #'char< :key #'car))
	))
