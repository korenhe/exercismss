(defpackage :word-count
  (:use :cl)
  (:export :count-words))
(in-package :word-count)

(defun split-words-manual (sentence)
  "Split a string into words manually, keeping apostrophes inside words."
  (let ((words '())         ; list to store words
        (current "")        ; accumulate current word
        (len (length sentence)))
    (loop for i from 0 below len
          for c = (char sentence i)
          do (if (or (alphanumericp c) (char= c #\'))
                 ;; if character is letter/digit/apostrophe, add to current word
                 (setf current (concatenate 'string current (string c)))
                 ;; else separator: end current word
                 (when (> (length current) 0)
                   (push current words)
                   (setf current ""))))

    ;; push last word if any
    (when (> (length current) 0)
      (push current words))
    words))

(defun count-words (sentence)
  (let* (
		 (wordtable (make-hash-table :test #'equal))
		 )
	(loop for xword in (split-words-manual (string-downcase sentence))
		  for word = (string-right-trim '(#\') (string-left-trim '(#\') xword))
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
