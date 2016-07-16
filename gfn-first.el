;;; gfn-first.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  

;; Author: gfngfn
;; Keywords:
(provide 'gfn-first)

(defun hello-world ()
  (interactive)
  (message "%s" "Hello World!"))

(defun gfn-insert-strong-line (title)
  (interactive "stitle: ")
  (insert (format ";; ==== ==== ==== ==== %s ==== ==== ==== ====\n" title)))

(defun gfn-insert-line (title)
  (interactive "stitle: ")
  (insert (format ";; ---- ---- %s ---- ----\n" title)))

(defun gfn-deal-single-letter-in-macro (str)
  (if (equal str "") nil
    (let ((ch (substring str 0 1)) (aft (substring str 1)))
      (cond ((equal ch "\\") (gfn-deal-escaped-letter-in-macro aft))
	    (t
	      (insert ch)
	      (gfn-deal-single-letter-in-macro aft))
	    ))))

(defun gfn-deal-escaped-letter-in-macro (str)
  (if (equal str "") nil
    (let ((ch (substring str 0 1)) (aft (substring str 1)))
      (progn
	(cond ((equal ch "n") (insert "\n"))
              ((equal ch "_") (next-line))
	      ((equal ch "^") (previous-line))
	      ((equal ch ">") (forward-char))
	      ((equal ch "<") (backward-char))
	      ((equal ch "d") (delete-char 1))
	      ((equal ch "\\") (insert "\\"))
	      (t (message "[ERROR in gfn-first] undefined escaped character.")))
	(gfn-deal-single-letter-in-macro aft)))))

(defun gfn-macro-sub (str num)
  (if (<= num 0) nil (progn (gfn-deal-single-letter-in-macro str) (gfn-macro-sub str (1- num)))))

(setq *last-gfn-macro* "")
(defun gfn-macro (str num)
  (interactive "scommand: \nniteration: ")
  (if (equal str "") (gfn-macro-sub *last-gfn-macro* num)
    (progn
      (setq *last-gfn-macro* str)
      (gfn-macro-sub str num))))

;(add-hook 'post-command-hook 'gfn-get-characters)

(defun gfn-get-characters ()
  (let ((chb (char-before (point))) (cha (char-after (point))))
    (message (format "before: %s after: %s" chb cha))))
