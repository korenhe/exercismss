(defpackage :pig-latin
  (:use :cl)
  (:export :translate))

(in-package :pig-latin)

(defun vowel-p (ch) (if (find ch '(#\a #\e #\i #\o #\u)) t))
(defun consonant-p (ch) (if (and (alpha-char-p ch) (not (or (char= ch #\y) (vowel-p ch)))) t))

(defun rule1-p (phrase)
  (let* ((len (length phrase))
         (first (if (> len 0) (aref phrase 0)))
         (first-deux (if (> len 1) (subseq phrase 0 2)))
         )
    (if (or (vowel-p first) (find first-deux '("xr" "yt") :test #'equal))
        t)))

(defun starts-consonants-count (phrase)
  (loop for i from 0
        for x across phrase
        until (not (consonant-p x))
        finally (return i)))

(defun translate-word (phrase)
  (cond
    ;; rule 1
    ((rule1-p phrase)
     (format t "rule 1 triggered")
     (concatenate 'string phrase "ay"))
    ;; rule 2
    ((consonant-p (aref phrase 0))
     (format t "rule 2 triggered")
     (let* ((cc (starts-consonants-count phrase))
            (body (subseq phrase cc))
            )
       ;; rule 3
       (if (and (> (length body) 0) (and (char= (aref phrase (1- cc)) #\q) (char= (aref body 0) #\u)))
           (apply #'concatenate 'string
                  (list (subseq body 1) (subseq phrase 0 cc) "uay"))
           (apply #'concatenate 'string
                  (list body (subseq phrase 0 cc) "ay")))
       ))
    ;; rule 4 (starts with y)
    ((char= #\y (aref phrase 0)) (concatenate 'string (subseq phrase 1) "yay"))
    (t phrase)))

(defun translate (phrase)
  (format nil "~{~A~^ ~}" (mapcar #'translate-word (uiop:split-string phrase))))
