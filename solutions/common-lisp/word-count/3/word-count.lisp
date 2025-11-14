(defpackage :word-count
  (:use :cl)
  (:export :count-words))
(in-package :word-count)

(defun split-words-manual (sentence)
  "Split a string into words manually, keeping apostrophes inside words."
  (flet ((push-wl-to-list(wl words)
		   (push (coerce (nreverse wl) 'string) words)
		   words
		   ))
	(let ((words '())         ; list to store words
		  (current '())        ; accumulate current word
		  (len (length sentence)))
	  (loop for i from 0 below len
			for c across sentence
			do (if (or (alphanumericp c) (char= c #\'))
				   (push c current)

				   (when (> (length current) 0)
					 (setf words (push-wl-to-list current words))
					 (setf current '()))))

	  (when (> (length current) 0)
		(setf words (push-wl-to-list current words)))
	  words)))

(defun count-words (sentence)
  (let* (
		 (wordtable (make-hash-table :test #'equal))
		 )
	(loop for xword in (split-words-manual (string-downcase sentence))
		  for word = (string-trim '(#\') xword)
		  when (> (length word) 0)
			do (setf (gethash word wordtable)
					 (1+ (gethash word wordtable 0))
					 )
		  )
	(let (result)
      (maphash (lambda (key value)
                 (push (cons key value) result))
               wordtable)
      result))
  )
