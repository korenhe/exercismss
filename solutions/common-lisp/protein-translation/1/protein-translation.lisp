(defpackage :protein-translation
  (:use :cl)
  (:export :proteins
           :invalid-protein))

(in-package :protein-translation)

(defun get-codon (codon)
  (cond
    ((find codon #("AUG") :test #'equal) "Methionine")
    ((find codon #("UUU" "UUC") :test #'equal) "Phenylalanine")
    ((find codon #("UUA" "UUG") :test #'equal) "Leucine")
    ((find codon #("UCU" "UCC" "UCA" "UCG") :test #'equal) "Serine")
    ((find codon #("UAU" "UAC") :test #'equal) "Tyrosine")
    ((find codon #("UGU" "UGC") :test #'equal) "Cysteine")
    ((find codon #("UGG") :test #'equal) "Tryptophan")
    ((find codon #("UAA" "UAG" "UGA") :test #'equal) "STOP")
    (t (error 'invalid-protein))
    )
  )

(define-condition invalid-protein (error)
  ()
  (:documentation "Raised when the RNA sequence is invalid or incomplete."))

(defun proteins (strand)
  (loop
    with last = nil
    for i from 0
    repeat (length strand)
    when (zerop (mod i 3))
      if (<= (+ i 3) (length strand))
        do (setf last (get-codon (subseq strand i (+ i 3))))
        and when (string/= "STOP" last)
              collect last
        end
      else
        do (progn
           (error 'invalid-protein))

    until (string= "STOP" last)
    )
  )
