(defpackage :meetup
  (:use :cl)
  (:export :meetup))

(in-package :meetup)

(defun get-dow (month year day)
  (multiple-value-bind (s m h d mo y dow)
    (decode-universal-time
     (encode-universal-time 0 0 0 day month year))
    (declare (ignore s m h d mo y))
    (case dow
      (0 :monday)
      (1 :tuesday)
      (2 :wednesday)
      (3 :thursday)
      (4 :friday)
      (5 :saturday)
      (6 :sunday)
      )
    ))

(defun leap-year (year)
  (cond
    ((zerop (mod year 400)) t)
    ((zerop (mod year 100)) nil)
    ((zerop (mod year 4)) t)
    (t nil)
    )
  )

(defun get-month-days (month year)
  (cond
    ((find month '(1 3 5 7 8 10 12)) 31)
    ((/= month 2) 30)
    (t (if (leap-year year) 29 28))
    )
  )

(defun meetup (month year dayofweek week)
  (loop
    with weekn = 0
    with last = 0
    for i from 1 to (get-month-days month year)
    for dow = (get-dow month year i)
    when (equal dow dayofweek)
      do (progn
           (incf weekn)
           (setf last i)
           (case week
             (:first (if (= weekn 1) (return-from meetup (list year month i))))
             (:second (if (= weekn 2) (return-from meetup (list year month i))))
             (:third (if (= weekn 3) (return-from meetup (list year month i))))
             (:fourth (if (= weekn 4) (return-from meetup (list year month i))))
             (:teenth (if (and (>= i 13) (<= i 19)) (return-from meetup (list year month i))))
             ))
    finally(return (list year month last))
    )
  )
