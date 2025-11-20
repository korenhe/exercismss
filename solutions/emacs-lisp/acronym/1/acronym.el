;;; acronym.el --- Acronym (exercism)  -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:


(defun acronym (phrase)
  (let* ((cleaned (remove-if-not (lambda (x)
				   (if
					   (or (or (and (>= x ?a) (<= x ?z))
						   (and (>= x ?A) (<= x ?Z))
						   ) (or (char-equal x ?-) (char-equal x ? )))
					   t)
				   )
								phrase)))
	(coerce (mapcar (lambda (x) (upcase (aref x 0))) (split-string cleaned "[ -]+")) 'string)
	)
  )

(provide 'acronym)
;;; acronym.el ends here
