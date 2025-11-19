(defpackage :two-bucket
  (:use :cl)
  (:export :measure))

(in-package :two-bucket)

(defun one-step-buckets (bucket-one-max bucket-one-current
                         bucket-two-max bucket-two-current
                         goal start-bucket count memo result)

  (case start-bucket
    (:one (if (and (zerop bucket-one-current) (= bucket-two-max bucket-two-current))
              (return-from one-step-buckets nil)
              ))
    (:two (if (and (zerop bucket-two-current) (= bucket-one-max bucket-one-current))
              (return-from one-step-buckets nil)
              ))
    )

  ;; check finish
  (if (or (= bucket-one-current goal) (= bucket-two-current goal))
      (progn
        ;;(format t "finish on (~A, ~A) count=~A~%" bucket-one-current bucket-two-current count)
        (if (or (null (car result)) (< count (cdr (assoc :moves (car result)))))
            (setf (car result) (list
                                (cons :moves count)
                                (cons :goal-bucket (if (= bucket-one-current goal) :one :two))
                                (cons :other-bucket (if (= bucket-one-current goal) bucket-two-current bucket-one-current))
                          )))
      ))

  ;; skip scanned
  (let* ((old-count (gethash (list bucket-one-current bucket-two-current) memo)))
    (if (and old-count (< old-count count))  (return-from one-step-buckets nil)))

  ;; cache
  (setf (gethash (list bucket-one-current bucket-two-current) memo) count)

  ;; action 1, pour
  (let* ((space (- bucket-two-max bucket-two-current))
         (pour (min bucket-one-current space))
         )
    (if (> pour 0) (one-step-buckets
                    bucket-one-max (- bucket-one-current pour) bucket-two-max (+ bucket-two-current pour) goal start-bucket (1+ count)
                    memo result
                    )
    ))

  ;; action 2, empty
  (if (> bucket-one-current 0)
      (one-step-buckets bucket-one-max 0 bucket-two-max bucket-two-current goal start-bucket (1+ count)
                        memo result))

  ;; action 3, fill
  (if (< bucket-one-current bucket-one-max)
      (one-step-buckets bucket-one-max bucket-one-max bucket-two-max bucket-two-current goal start-bucket (1+ count)
                        memo result
                        ))

  ;; action 1, pour
  (let* ((space (- bucket-one-max bucket-one-current))
         (pour (min bucket-two-current space))
         )
    (if (> pour 0) (one-step-buckets bucket-one-max (+ bucket-one-current pour) bucket-two-max (- bucket-two-current pour) goal start-bucket (1+ count)
                                     memo result))
    )

  ;; action 2, empty
  (if (> bucket-two-current 0)
      (one-step-buckets bucket-one-max bucket-one-current bucket-two-max 0 goal start-bucket (1+ count)
                        memo result))

  ;; action 3, fill
  (if (< bucket-two-current bucket-two-max)
      (one-step-buckets bucket-one-max bucket-one-current bucket-two-max bucket-two-max goal start-bucket (1+ count)
                        memo result))
  )

(defun measure (bucket-one bucket-two goal start-bucket)
  "Function to solve the two-bucket puzzle, if possible, when given the capacities
of both buckets, a goal, and which bucket to start with.  Returns an alist of moves
required to reach the goal, the name of the bucket that reach the goal, and the
amount of water left over in the other bucket."
  (let* (
         (memo (make-hash-table :test #'equal))
         (result (list nil))
         )
    (one-step-buckets bucket-one 0 bucket-two 0 goal start-bucket 0 memo result)
    (car result)
    )
  )
