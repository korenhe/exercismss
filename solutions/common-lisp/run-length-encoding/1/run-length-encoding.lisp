(defpackage :run-length-encoding
  (:use :cl)
  (:export :encode
           :decode))

(in-package :run-length-encoding)

(defun encode-word (count last)
  (if (= 1 count) (format nil "~A" last)
      (format nil "~A~A" count last)
      )
  )

(defun encode (plain)
  (apply #'concatenate 'string (loop
    with last = nil
    with count = 0
    for x across plain
    if (null last)
      do (progn
           (setf last x)
           (setf count 1)
           )
    else
      if (char/= last x)
        collect (encode-word count last) into result
        and
          do (progn
               (setf count 1)
               (setf last x)
               )
       else do (incf count)
    finally(progn
             (if (> count 0)
                 (setf result (append result (list (encode-word count last)))))
             (return result)
             )
    ))
  )

(defun decode-word (count x)
  (coerce (loop repeat (max 1 count) collect x) 'string)
  )

(defun decode (compressed)
  (apply #'concatenate 'string (loop
    with count = 0
    for x across compressed
    if (digit-char-p x)
      do (setf count (+ (* count 10) (- (char-code x) (char-code #\0))))
    else
      collect (decode-word count x) and
      do (setf count 0)
      ))
  )
