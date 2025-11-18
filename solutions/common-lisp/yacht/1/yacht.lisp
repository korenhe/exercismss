(defpackage :yacht
  (:use :cl)
  (:export :score))
(in-package :yacht)

(defun score (scores category)
  "Returns the score of the dice for the given category."
  (case category
    (:ones
     (reduce (lambda(acc x) (if (= 1 x) (1+ acc) acc)) scores :initial-value 0))
    (:twos
     (reduce (lambda(acc x) (if (= 2 x) (+ x acc) acc)) scores :initial-value 0))
    (:threes
     (reduce (lambda(acc x) (if (= 3 x) (+ x acc) acc)) scores :initial-value 0))
    (:fours
     (reduce (lambda(acc x) (if (= 4 x) (+ x acc) acc)) scores :initial-value 0))
    (:fives
     (reduce (lambda(acc x) (if (= 5 x) (+ x acc) acc)) scores :initial-value 0))
    (:sixes
     (reduce (lambda(acc x) (if (= 6 x) (+ x acc) acc)) scores :initial-value 0))
    (:full-house
     (let* ((cleaned (remove-duplicates scores)))
       (if (and (= 2 (length cleaned))
                (let* ((ff (first cleaned))
                       (cc (count ff scores))
                       )
                  (or (= cc 2) (= cc 3))
                  ))
           (reduce #'+ scores :initial-value 0)
           0
           )))
    (:four-of-a-kind
     (let* ((cleaned (remove-duplicates scores))
            (len (length cleaned))
            )
       (if (and (or (= 2 len) (= 1 len))
                (let* ((ff (first cleaned))
                       (cc (count ff scores))
                       )
                  (or (= cc 1) (= cc 4) (= cc 5))
                  ))
           (loop for c in cleaned
                 for cc = (count c scores)
                 when (> cc 3)
                   return (* c 4)
                 )
           0
           )))
    (:little-straight
     (let* ((cleaned (sort (remove-duplicates scores) #'<)))
       (if (and (= 5 (length cleaned)) (and (= (first cleaned) 1) (= (car (last cleaned)) 5)))
           30
           0
           )))
    (:big-straight
     (let* ((cleaned (sort (remove-duplicates scores) #'<)))
       (if (and (= 5 (length cleaned)) (and (= (first cleaned) 2) (= (car (last cleaned)) 6)))
           30
           0
           )))
    (:choice (reduce #'+ scores :initial-value 0))
    (:yacht
     (let* ((cleaned (remove-duplicates scores)))
       (if (= 1 (length cleaned))
           50
           0)))
    )
  )
