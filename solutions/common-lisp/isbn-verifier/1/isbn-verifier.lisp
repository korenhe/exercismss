(defpackage :isbn-verifier
  (:use :cl)
  (:export :validp))

(in-package :isbn-verifier)

(defun validp (isbn)
  ;; validate and extract
  (let* (
         (serial  (loop for el across isbn
                        if (digit-char-p el)
                          collect (- (char-code el) (char-code #\0))
                        else
                          if (char= #\x (char-downcase el))
                            collect 10
                        else if (char/= el #\-)
                               do (return-from validp nil)

                        )
                  )
         )

    ;; validate 2
    (if (/= 10 (length serial)) (return-from validp nil))
    (mapcar (lambda (x) (if (> x 9) (return-from validp nil))) (butlast serial))

    ;; validate 3
    (zerop (mod (loop
      for i from 10 above 0
      for n in serial
      sum (* i n)
      ) 11))
    )
  )
