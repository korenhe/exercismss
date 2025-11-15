(defpackage :grade-school
  (:use :cl)
  (:export :make-school :add :roster :grade))

(in-package :grade-school)

(defun make-school ()
  (make-hash-table :test 'equal))

(defun add (school name grade)
  (if (gethash name school) nil
      (setf (gethash name school) grade)))

(defun roster (school)
  (let* ((res '()))
    (maphash (lambda (k v) (push (cons v k) res)) school)
    (mapcar #'cdr (sort res (lambda (a b) (if (= (car a) (car b))
                                (string< (cdr a) (cdr b))
                                (< (car a) (car b))
                                ))))
    ))

(defun grade (school grade)
  (let* ((res '()))
    (maphash (lambda (k v) (if (= v grade) (push k res))) school)
    (sort res #'string<)
    ))
