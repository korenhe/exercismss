(defpackage :anagram
  (:use :cl)
  (:export :anagrams-for))

(in-package :anagram)

(defun anagrams-for (subject candidates)
  "Returns a sublist of candidates which are anagrams of the subject."
  (loop for cand in candidates
		when (string/= (string-downcase subject) (string-downcase cand))
		  when (string= (sort (string-downcase (copy-seq subject)) #'char<) (sort (string-downcase (copy-seq cand)) #'char<))
			collect cand
		)
  )
