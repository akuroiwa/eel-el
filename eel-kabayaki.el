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
  "Skip Python comment.

Move point N characters forward (backward if N is negative)."
  (interactive "p")
  (while (eel-python-comment-or-string-p)
    (forward-char n)
    (point)))				;Return value.

;;;###autoload
(defun eel-character-address-of-next-start-of-comment (&optional n)
  "Character address of next start of Python comment.

Move point N characters forward (backward if N is negative)."
  (interactive "p")
  (while (not (eel-python-comment-or-string-p))
    (forward-char n)
    (point)))				;Return value of point to repeat `while' TEST.
					;https://www.emacswiki.org/emacs/ElispCookbook

;;;###autoload
(defun eel-replace-java-comment-with-python-one (beg end)
  "Replace Java comment delimiters with Python one between BEG and END."
  (interactive "r")
  (let (
	(re-alist '("/\\*+\\(.*\\(.*\n\\)*?.*\\)\\*+/" . "\'\'\'\\1\'\'\'"))) ;match multiple lines.
    (python-mode)
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char (point-min))
	(while (re-search-forward (car re-alist) nil t)
	  (replace-match (cdr re-alist) nil nil)
	  (goto-char (point-min))
	  (while (re-search-forward "^\\([ \t]\\*\\)\\(.*\n\\)" nil t)
	    (replace-match "\\2" nil nil)))
	(goto-char (point-min))
	(while
	    (progn
	      (forward-char)
	      (not (eel-python-comment-or-string-p)))
	  (while (looking-back "//")
	    (replace-match "##")))))))

;;;###autoload
(defun eel-convert-from-java-to-python (beg end)
  "Convert code from Java to Python in the region between BEG and END.

This function has been left unfinished."
  (interactive "r")
  (python-mode)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (eel-replace-java-comment-with-python-one (point-min) (point-max)))))

(provide 'eel-kabayaki)
;;; eel-kabayaki.el ends here
