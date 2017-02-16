;;; eel.el --- Eel's Emacs Lisp.
;;
;; Author: Akihiro Kuroiwa
;; Maintainer: Akihiro Kuroiwa
;; Created: 30 May 2016
;; Keywords: regexp, comments
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

;; My personal functions.

;; (setq eel-list '("Eel\'s" "emacs" "lisp"))
;; (eel-evaluate-recursive-acronym eel-list)

;; (eel-join-lines)
;; Join lines region or `yank' and join lines.

;; (eel-just-one-space)
;; `just-one-space' in the region between BEG and END.
;; With prefix argument ARG, stay indented.

;; (eel-convert-from-java-to-python)
;; Convert code from Java to Python in the region between BEG and END.
;; This function has been left unfinished
;; and is available in `eel-kabayaki.el'.

;; (eel-delete-blank-lines)
;; Delete blank lines in region between BEG and END.

;; (eel-toggle-double-quote)
;; Create double-quoted string or remove double quotes in region or sexp.

;;; Code:

(require 'cl-lib)
(require 'thingatpt)

;;;###autoload
(defun eel-evaluate-recursive-acronym (acronym-list &optional arg)
  "Using dolist, evaluate the recursive acronym of ACRONYM-LIST.

With prefix argument ARG, insert the abbreviation at `region-end'
or at point.

Non-interactively:
\(setq eel-list '\(\"Eel\\'s\" \"emacs\" \"lisp\"\)\)
\(eel-evaluate-recursive-acronym eel-list\)

If you want to insert the abbreviation at point:
\(defun eel-test \(eel-list\)
  \"Evaluate EEL-LIST.\"
  \(interactive
   \(list
    \(split-string \(read-string \":\"\)\)\)\)
  \(eel-evaluate-recursive-acronym eel-list t\)\)"
  (interactive
   (let* (
	  (acronym-string
	   (if (use-region-p)
	       (buffer-substring-no-properties (region-beginning) (region-end))
	     (buffer-substring-no-properties (mark) (point))))
	  (acronym-list
	   (split-string (read-string
			  (format "Recursive acronym (default %s): " acronym-string)
			  acronym-string nil acronym-string))))
     (list acronym-list current-prefix-arg)))
  (let (value)  ; make sure list starts empty
    (dolist (element acronym-list value)
      (setq value (concat value (substring element 0 1))))
    (save-excursion
      (save-restriction
	(cond ((use-region-p)
	       (narrow-to-region (region-beginning) (region-end)))
	      ((not (called-interactively-p 'interactive))
	       (narrow-to-region (point) (point)))
	      (t
	       (narrow-to-region (mark) (point))))
	(when arg
	  (goto-char (point-max))
	  (insert ?\  ?\( (upcase value) ?\)))))
    (if (string-match (upcase value) (upcase (car acronym-list)))
	(message "%s is the recursive acronym." (upcase value))
      (message "%s is NOT the recursive acronym." (upcase value)))
    (upcase value)))			;Return value unless called interactively.

;;;###autoload
(defun eel-join-lines (&optional arg one-space stay-indented)
  "Join lines region or `yank' and join lines.

With ARG nil or 1, insert the most recent kill and join lines.
With argument N, reinsert the Nth most recent kill and join lines.

With argument ONE-SPACE, does not join lines and execute the command such as\
`delete-horizontal-space' `just-one-space' and `delete-trailing-whitespace' \
including ideographic space.
With argument STAY-INDENTED, stay indented.
These two arguments are called from `eel-just-one-space'.

This is the reuse of `keyword-search-get-query' in `keyword-search.el'
and based on the code of `xah-replace-regexp-pairs-region' in `xah-replace-pairs.el'."
  (interactive "*P")
  (let (
	(re-alist
	 '(
	   ("\\(\\s(\\) \\(\\s(\\)" . "\\1\\2")
	   ("\\(\\s)\\) \\(\\s)\\)" . "\\1\\2")
	   ("\\(\\s(\\) \\(\\s(\\)" . "\\1\\2")
	   ("\\(\\s)\\) \\(\\s)\\)" . "\\1\\2")
	   ))
	(one-space-alist
	 '(
	   ("[\u3000 \t]+" . " ")
	   ("^\s-*\\|\s-*$" . "")
	   ))
	(stay-indented-alist
	 '(
	   ("\u3000" . " ")
	   ("\\_>[ \t]+\\_<" . " ") ;start\end of symbol
	   ("[ \t]+$" . "")
	   ))
	(original-alist
	 '(
	   ("[\u3000 \t\n]+" . " ")
	   ("^\s-*\\|\s-*$" . "")
	   ))
	)
    (save-restriction
      (when arg
      	(yank arg))
      (if (use-region-p)
      	  (narrow-to-region (region-beginning) (region-end))
	(narrow-to-region (mark) (point)))
      (cond
       ((and one-space
	     (eq nil stay-indented))
	(setq re-alist
	      (cl-concatenate 'list one-space-alist re-alist)))
       ((or stay-indented
       	    (and one-space stay-indented))
	(setq re-alist
	      (cl-concatenate 'list stay-indented-alist re-alist)))
       (t
	(setq re-alist
	      (cl-concatenate 'list original-alist re-alist))))
      (mapc
       (lambda (element)
	 (goto-char (point-min))
	 (while (re-search-forward (car element) nil t)
	   (replace-match (cdr element) nil nil)))
       re-alist))))

;;;###autoload
(defun eel-just-one-space (beg end &optional arg)
  "`just-one-space' in the region between BEG and END.

With prefix argument ARG, stay indented."
  (interactive "r\nP")
  (save-excursion
    (if arg
	(eel-join-lines nil nil t)
      (eel-join-lines nil t nil))))

;;;###autoload
(defun eel-delete-blank-lines (beg end &optional arg)
  "Delete blank lines in region between BEG and END.

With prefix argument ARG, delete all surrounding blank lines, leaving just one \
based on `delete-blank-lines'."
  (interactive "r\nP")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (eel-just-one-space (point-min) (point-max) t)
      (goto-char (point-min))
      (while (re-search-forward "[ \t]*\n[ \t]*$" nil t)
	(if arg
	    (progn
	      (newline 2)
	      (delete-blank-lines))
	  (delete-blank-lines)
	  (delete-blank-lines)))
      (when (not arg)
	(goto-char (point-min))
	(when (and (bolp) (eolp))
	  (delete-blank-lines))))))

;;;###autoload
(defun eel-delete-html-tag (beg end)
  "Delete html tag in region between BEG and END."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "<.*?>" nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      (eel-delete-blank-lines (point-min) (point-max))
      (eel-just-one-space (point-min) (point-max)))))

(defun eel-toggle-double-quote-function (beg end)
  "Subroutine of `eel-toggle-double-quote'.

`narrow-to-region' BEG END."
  (save-restriction
    (narrow-to-region beg end)
    (cond ((and
	    (eq (char-syntax (char-after (point-min))) ?\")
	    (eq (char-syntax (char-before (point-max))) ?\")
	    )
	   (goto-char (point-min))
	   (delete-char 1)
	   (goto-char (point-max))
	   (delete-char -1))
	  (t
	   (goto-char (point-min))
	   (insert ?\")
	   (goto-char (point-max))
	   (insert ?\")))))

;;;###autoload
(defun eel-toggle-double-quote (&optional arg)
  "Create double-quoted string or remove double quotes in region or sexp.

With ARG 1, insert the most recent kill with double quotes.
With argument N, reinsert the Nth most recent kill with double quotes.

This is based on the code of `smartparens.el' and `toggle-quotes.el'."
  (interactive "*P")
  (save-excursion
    (cond
     ((use-region-p)
      (let (
	    (beg (region-beginning))
	    (end (region-end))
	    )
	(eel-toggle-double-quote-function beg end)))
     ((not current-prefix-arg)
      (cond
       ((nth 3 (syntax-ppss))
	(goto-char (nth 8 (syntax-ppss))))
       ((thing-at-point-looking-at "\"\\{2,\\}.*?\"\\{2,\\}") ;Defeat nested double quotes.
	(progn
	  (goto-char (match-beginning 0))
	  (skip-syntax-forward "\"")
	  (backward-char)))
       ((and
	 (cl-oddp (how-many "\"" (nth 8 (syntax-ppss)) (point)))
	 (re-search-backward "\"" (nth 8 (syntax-ppss)) t)
	 (nth 4 (syntax-ppss)))
	(goto-char (match-beginning 0))))
      (let (
	    (beg (beginning-of-thing 'sexp))
	    (end (end-of-thing 'sexp))
	    )
	(eel-toggle-double-quote-function beg end)))
     (t
      (insert ?\")
      (yank arg)
      (insert ?\")))))

;;;###autoload
(defun eel-add-number-grouping (number &optional separator)
  "Add commas to NUMBER and return it as a string.
Optional SEPARATOR is the string to use to separate groups.
It defaults to a comma.

This is based on the code snippets written by Alex Schroeder at:
https://www.emacswiki.org/emacs/AddCommasToNumbers"
  (interactive
   (let
       ((number (if (use-region-p)
		    (buffer-substring-no-properties (region-beginning) (region-end))
		  (number-to-string (read-number "Number: ")))))
     (list number)))
  ;; (let ((num (number-to-string number))
  (let* ((num number)
	 (op (or separator ","))
	 (re-string
	  (if (string-match "\\s." num)	;Decimal point.
	      "\\(.*[0-9]\\)\\([0-9]\\{3\\}.*\\)\\(\\s..*\\)"
	    "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)")))
    ;; (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
    (while (string-match re-string num)
      (setq num (concat
		 (match-string 1 num) op
		 (match-string 2 num)
		 (match-string 3 num))))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (when (called-interactively-p 'interactive) (insert num))
    num))				;Return value unless called interactively.

;;;###autoload
(defun eel-concatenate-insert-register (registers &optional delimiter)
  "Insert and concatenate contents of register REGISTERS.
\(REGISTERS is characters without white-space delimiter.\)

Default DELIMITER of each content is nil.
See `copy-to-register' and `insert-register'."
  (interactive
   (let (
	 (registers (string-to-list (read-string "Registers without white-space delimiter: ")))
	 (delimiter (read-string "Delimiter of each content: " nil nil nil))
	 )
     (list registers delimiter)))
  (save-excursion
    (mapc
     (lambda (element)
       (insert-register element t)
       (insert delimiter))
     registers)))

(provide 'eel)
;;; eel.el ends here
