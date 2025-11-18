(defpackage :affine-cipher
  (:use :cl)
  (:export :encode
           :decode))

(in-package :affine-cipher)

(defun coprime-p (a m)
  (loop for i from 2 to a
        if (and (zerop (mod a i)) (zerop (mod m i)))
          do (return-from coprime-p nil)
        ) t)

(defun encode (plaintext a b)
  (let ((m 26))
    (if (coprime-p a m)
        (let* (
               (stagingstr
                 (map 'string (lambda (x)
                           (cond
                             ((lower-case-p x)
                              (code-char (+ (char-code #\a) (mod (+ (* a (- (char-code x) (char-code #\a))) b) m))))
                             ((upper-case-p x)
                              (code-char (+ (char-code #\a) (mod (+ (* a (- (char-code x) (char-code #\A))) b) m))))
                             ((digit-char-p x) x)
                             )
                           )
                         (remove-if-not (lambda (x)
                                          (if (or (alpha-char-p x) (digit-char-p x)) t)
                                          )
                                        plaintext))))

          ;; insert space in fixed interval
          (string-right-trim '(#\Space) (coerce (loop for s across stagingstr
                for i from 1
                collect s
                if (zerop (mod i 5))
                  collect #\Space
                ) 'string))))))

(defun get-mmi (a m)
  (loop for i from 1
        until (= 1 (mod (* a i) m))
        finally (return i)))

(defun decode (ciphertext a b)
  (let ((m 26))
    (if (coprime-p a m)
    (map 'string (lambda (x)
                   (code-char
                    (
                     cond
                      ((lower-case-p x)
                       (+ (char-code #\a) (mod (* (get-mmi a m) (- (- (char-code x) (char-code #\a)) b)) m)))
                      ((digit-char-p x) (char-code x))
                       ))
                   ) (remove-if-not (lambda (x)
                                      (if (or (alpha-char-p x) (digit-char-p x)) t)
                                      )ciphertext)))))
