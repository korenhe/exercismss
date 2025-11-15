(defpackage :robot-simulator
  (:use :cl)
  (:export :+north+ :+east+ :+south+ :+west+ :execute-sequence
           :robot :robot-position :robot-bearing :make-robot))

(in-package :robot-simulator)

(defparameter +north+ '(0 . 1))
(defparameter +east+ '(1 . 0))
(defparameter +south+ '(0 . -1))
(defparameter +west+ '(-1 . 0))

(defparameter *directions* (vector
                            +north+
                            +east+
                            +south+
                            +west+
                            ))
(defstruct robot
  (position '(0 . 0))       ; default to (0 . 0)
  (bearing +north+))        ; default to the symbol +north+

(defun robot-turn-left (rob)
  (let* (
         (dirpos (position (robot-bearing rob) *directions* :test 'equal))
         )
    (setf (robot-bearing rob) (svref *directions* (mod (1- dirpos) 4)))
    )
  )

(defun robot-turn-right (rob)
  (let* (
         (dirpos (position (robot-bearing rob) *directions* :test 'equal))
         )
    (setf (robot-bearing rob) (svref *directions* (mod (1+ dirpos) 4)))
    )
  )

(defun robot-turn-advance (rob)
  (let* (
         (x (car (robot-position rob)))
         (y (cdr (robot-position rob)))
         (nx (+ x (car (robot-bearing rob))))
         (ny (+ y (cdr (robot-bearing rob))))
         )
    (setf (robot-position rob) (cons nx ny))
    )
  )

(defun execute-sequence (rob seq)
  (loop for s across seq
        do (case s
             (#\R (robot-turn-right rob))
             (#\L (robot-turn-left rob))
             (#\A (robot-turn-advance rob))
             )
        finally (return rob)
        )
  )
