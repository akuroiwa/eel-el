;;; eel-kabayaki.el --- Roasted eel.el.
;;
;; Author: Akihiro Kuroiwa
;; Maintainer: Akihiro Kuroiwa
;; Created: 4 Jan 2017
;; Keywords: regexp, comments, java, python
;; Package-Requires: ((python-mode))
;; URL: https://github.com/akuroiwa/eel-el
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This code is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; This code is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Please see commentary of `eel.el'.

;;; Code:

(require 'python-mode)

(defun eel-python-comment-or-string-p ()
  "Return non-nil if point is on Python comment, string, or docstring.

This function does not change the match data like `string-match-p'
with `inhibit-changing-match-data'."
  (eq t (or
	 (looking-at-p "\\(\"\"\"\\|'''\\|\"\\|'\\)") ;`py-string-delim-re' in `python-mode'.
	 (looking-at-p "#+")
	 (nth 3 (syntax-ppss))
	 (nth 4 (syntax-ppss)))))

;;;###autoload
(defun eel-skip-python-comment-forward (&optional n)
  "Skip Python comment, string or docstring.

Move point N characters forward (backward if N is negative)."
  (interactive "p")
  (while (eel-python-comment-or-string-p)
    (forward-char n)
    (point)))				;Return value.

;;;###autoload
(defun eel-character-address-of-next-start-of-comment (&optional n)
  "Character address of next start of Python comment, string or docstring.

Move point N characters forward (backward if N is negative)."
  (interactive "p")
  (while (not (eel-python-comment-or-string-p))
    (forward-char n)
    (point)))				;Return value of point to repeat `while' TEST.
					;https://www.emacswiki.org/emacs/ElispCookbook

;;;###autoload
(defun eel-replace-java-comment-start-with-python-one (beg end)
  "Replace Java `comment-start' with Python one outside Python docstring \
in region between BEG and END or in whole buffer."
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))))
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
	(while (progn
		 (forward-char)
		 (eel-skip-python-comment-forward)
		 (not (looking-at-p "//")))) ;Stop at `comment-start' of Java.
	(delete-char 2)
	(insert "##")
	(eel-py--escape-doublequotes (point) (point-at-eol))))))

(defun eel-py--escape-doublequotes (start end)
  "Escape quote character including single quote or apostrophe \
between START and END.

This is based on the code `py--escape-doublequotes'."
  (let ((end (copy-marker end)))
    (save-excursion
      (goto-char start)
      (while (and (not (eobp)) (< 0 (abs (skip-chars-forward "^\'" end))))
	(when (eq (char-after) ?\')
	  (unless (py-escaped)
	    (insert "\\")
	    (forward-char 1))))
      (goto-char start)
      (while (and (not (eobp)) (< 0 (abs (skip-chars-forward "^\"" end))))
	(when (eq (char-after) ?\")
	  (unless (py-escaped)
	    (insert "\\")
	    (forward-char 1)))))))

;;;###autoload
(defun eel-replace-java-docstring-with-python-one (beg end)
  "Replace Java docstring delimiters with Python one between BEG and END \
or in whole buffer.

Quote character in docstring will be escaped.

This function has been left unfinished."
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))))
  (let (
	(re-alist '(
		    ("/\\*+\\(.*?\\)\\*+/" . "\"\"\"\\1\"\"\"") ;match one line.
		    ("/\\*+\\(.*?\\(.*\n\\)*?.*?\\)\\*+/" . "\"\"\"\\1\"\"\"") ;match multiple lines.
		    )))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(mapc
	 (lambda (element)
	   (goto-char (point-min))
	   (while (re-search-forward (car element) nil t)
	     (eel-py--escape-doublequotes (match-beginning 0) (match-end 0)))
	   (goto-char (point-min))
	   (while (re-search-forward (car element) nil t)
	     (replace-match (cdr element) nil nil)
	     (syntax-ppss-flush-cache (match-beginning 0))
	     (while (re-search-forward "^ \\*[^/]" nil t)
	       (replace-match ""))))
	 re-alist)))))

;;;###autoload
(defun eel-replace-java-class-with-python-one (beg end)
  "Replace Java class with Python one outside Python docstring in whole buffer \
or in region between BEG and END."
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))))
  (let (
	(re-alist '(
		    ("\\(public\\|private\\|protected\\) " . "")
		    ("class\\(.*?\\)\\(extends\\) \\(.*?\\) *\{" . "class\\1\(\\3\):\{")
		    ("class\\(.*?\\) *\{" . "class\\1:\{"))))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(mapc
	 (lambda (element)
	   (goto-char (point-min))
	   (while (not (eobp))
	     (while (progn
		      (forward-line)
		      (eel-skip-python-comment-forward)
		      (looking-at (car element)))
	       (replace-match (cdr element))
	       (goto-char (match-end 0))
	       (backward-char)
	       (when (char-equal ?\{ (char-after))
		 (delete-pair)))))
	 re-alist)))))

;;;###autoload
(defun eel-convert-from-java-to-python (beg end)
  "Convert code from Java to Python in the region between BEG and END.

This function has been left unfinished.
It doesn't work correctly since there are so many bugs.
Please select a region manually and execute each function."
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))))
  (python-mode)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (eel-replace-java-docstring-with-python-one (point-min) (point-max))
      (eel-replace-java-comment-start-with-python-one (point-min) (point-max))
      (eel-replace-java-class-with-python-one (point-min) (point-max)))))

(provide 'eel-kabayaki)
;;; eel-kabayaki.el ends here
