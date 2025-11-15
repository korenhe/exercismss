(defpackage :atbash-cipher
  (:use :cl)
  (:export :encode :decode))

(in-package :atbash-cipher)

(defun encode-inner (x)
  (code-char (+ (- 25 (- (char-code x) (char-code #\a))) (char-code #\a)))
  )

(defun encode (plaintext)
  (loop for xx across plaintext
		for x = (char-downcase xx)

		when (not (find x '(#\  #\. #\,)))
		  if (alpha-char-p x)
			collect (encode-inner x) into result
		  else
    		collect x into result

		when (= 5 (mod (length result) 6))
		  collect #\  into result

		finally (return (string-right-trim '(#\ ) (coerce result 'string)))
		)
  )

(defun decode (ciphertext)
  (loop for xx across ciphertext
		for x = (char-downcase xx)
		when (string/= " " x)
		  if (alpha-char-p x)
			collect (encode-inner x) into result
		else
		  collect x into result
		finally (return (coerce result 'string))
		)
  )
