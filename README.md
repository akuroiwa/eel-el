# eel-el

Eel's emacs lisp.

## Installation

You can install them with the following command.

* <kbd>M-x package-install-file [RET] eel-el/eel.el [RET]</kbd>
* <kbd>M-x package-install-file [RET] eel-el/eel-kabayaki.el [RET]</kbd>

## Usage

If you are Emacs beginner, please read Emacs Tutorial from Help menu.

* <kbd>C-h f eel-evaluate-recursive-acronym [RET]</kbd>
* <kbd>M-x describe-function [RET] eel-join-lines [RET]</kbd>

### eel.el

* eel-evaluate-recursive-acronym
* eel-join-lines
* eel-just-one-space
* eel-delete-blank-lines
* eel-delete-html-tag
* eel-toggle-double-quote
* eel-add-number-grouping
* eel-concatenate-insert-register
* eel-comment-kill

### eel-kabayaki.el

Convert from java to python.

It doesn't work correctly since there are so many bugs.  Please select a region manually and execute each function in order as follows \(but WITHOUT ANY WARRANTY\):

1. <kbd>M-x python-mode
2. <kbd>M-x eel-replace-java-docstring-with-python-one [RET]
3. <kbd>M-x eel-replace-java-comment-start-with-python-one [RET]
4. <kbd>M-x eel-replace-java-class-with-python-one [RET]

If you like a challenge, please execute a command as below:

* <kbd>M-x eel-convert-from-java-to-python [RET]

## Demo Video

* [PPAP with Emacs](https://youtu.be/iFVPRzeotHc)
