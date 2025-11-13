(defpackage :word-count
  (:use :cl)
  (:export :count-words))
(in-package :word-count)

(ql:quickload :cl-ppcre)

(defun split-words (sentence)
  "Split SENTENCE into words, keeping internal apostrophes."
  (cl-ppcre:all-matches-as-strings
   "[A-Za-z0-9]+('[A-Za-z0-9]+)*"
   sentence))

(defun count-words (sentence)
  (let* (
		 (wordtable (make-hash-table :test #'equal))
		 )
	(loop for word in (split-words (string-downcase sentence))
		  do (setf (gethash word wordtable)
				   (1+ (gethash word wordtable 0))
				   )
		  )
	(let (result)
      (maphash (lambda (key value)
                 (push (cons key value) result))
               wordtable)
      (nreverse result)))
  )
