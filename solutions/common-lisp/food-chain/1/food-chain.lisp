(defpackage :food-chain
  (:use :cl)
  (:export :recite))

(in-package :food-chain)

(defparameter *insects* #("fly" "spider" "bird" "cat" "dog" "goat" "cow" "horse"))
(defparameter *action* "wriggled and jiggled and tickled inside her")

(defparameter *exclaims* #("" "It wriggled and jiggled and tickled inside her."
                          "How absurd to swallow a bird!"
                          "Imagine that, to swallow a cat!"
                          "What a hog, to swallow a dog!"
                          "Just opened her throat and swallowed a goat!"
                          "I don't know how she swallowed a cow!"
                          "She's dead, of course!"
                          ))


(defun recite (start-verse end-verse)
  (format nil "~{~A~^~%~}" (loop
    for i from 0
    for insect across *insects*
    for exclaim across *exclaims*

    when (and (>= (1+ i) start-verse) (<= (1+ i) end-verse))
      collect (format nil "I know an old lady who swallowed a ~A." insect)
      and
          if (> i 0)
            collect (format nil "~A" exclaim) and
            if (< (1+ i) (length *insects*))
              append (loop
                       for j from i downto 1
                       if (= j 2)
                         collect (format nil "She swallowed the ~A to catch the ~A that ~A." (aref *insects* j) (aref *insects* (1- j)) *action*)
                       else
                         collect (format nil "She swallowed the ~A to catch the ~A." (aref *insects* j) (aref *insects* (1- j)))
                       )

    when (and (>= (1+ i) start-verse) (<= (1+ i) end-verse))
      if (< (1+ i) (length *insects*))
        collect (format nil "I don't know why she swallowed the fly. Perhaps she'll die.")
    when (and (>= (1+ i) start-verse) (< (1+ i) end-verse))
      collect ""
    ))
  )
