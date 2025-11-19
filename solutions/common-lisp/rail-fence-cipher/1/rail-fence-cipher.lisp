(defpackage :rail-fence-cipher
  (:use :cl)
  (:export :encode
           :decode))

(in-package :rail-fence-cipher)

(defun encode (msg rails)
  (coerce (mapcan (lambda (x) (reverse x))
          (loop
            with container = (make-list rails :initial-element '())
            with y = 0
            with direction = 1
            for tx across msg
            do (progn
                 (push tx (nth y container))
                 (cond
                   ((and (= 1 direction) (= y (1- rails))) (setf direction (- 0 direction)))
                   ((and (= -1 direction) (= y 0)) (setf direction (- 0 direction)))
                   )
                 (setf y (+ y direction))
                 )
            finally (return container)
            )
          ) 'string)
  )

(defun decode (msg rails)
  (let* ((container (make-list rails))
         (new-msg (copy-seq msg))
         (plan (mapcan
                (lambda (x) (reverse x))
                (loop
                  with y = 0
                  with direction = 1
                  repeat (length msg)
                  for x from 0
                  do (progn
                       (push x (nth y container))
                       (cond
                         ((and (= 1 direction) (= y (1- rails))) (setf direction (- 0 direction)))
                         ((and (= -1 direction) (= y 0)) (setf direction (- 0 direction)))
                         )
                       (setf y (+ y direction))
                       )
                  finally(return container)
                  ))
               )
         )
    (loop for i in plan
          for tx across msg
          do (setf (aref new-msg i) tx)
          )
    new-msg
    ))
