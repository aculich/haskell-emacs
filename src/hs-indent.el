;;; hs-indent.el — Predictable indentation.

;; Copyright (C) 2011 Chris Done

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'hs-macros)

(defvar hs-indent-bring-back-list
  '("module"
    "import"
    "type"
    "deriving"
    "hiding"))

(defun hs-indent (change)
  "Indent line(s) depending on what's above and below and where the point is at."
  (or (hs-indent-bring-backs)
      (hs-indent-data)
      (hs-indent-function-decl)
      (hs-indent-default-bring-back)))

(defun hs-indent-bring-backs ()
  "Bring back the indentation if there shouldn't be any indentation on this line."
  (where* 
   (when bring-back?
     (hs-indent-delete-line-indentation)
     t)
   ((bring-back? (hs-indent-previous-line-looking-at? choices))
    (choices (mapconcat 'identity hs-indent-bring-back-list "\\|")))))

(defun hs-indent-data ()
  "Indent for data."
  (where*
   (when keyword-match?
     (if (and (= 2 (hs-indent-level))
              (not (hs-indent-looking-at? "deriving")))
         (hs-indent-delete-line-indentation)
       (hs-indent-change-level 2))
     t)
   ((keyword-match? (and (hs-indent-previous-line-looking-at? "data")
                         (not (hs-indent-previous-line-contains? "deriving")))))))

(defun hs-indent-function-decl ()
  "Indent after function declaration."
  (when (hs-indent-previous-line-contains? "=")
    (hs-indent-change-level 2)))

(defun hs-indent-default-bring-back ()
  "If we don't know what to do, bring the indentation to the start of the line."
  (progn (hs-indent-delete-line-indentation)
         t))

(defun hs-indent-looking-at? (regex)
  "Are we looking at the regex on the previous line?"
  (goto-char (line-beginning-position))
  (hs-indent-skip-non-word-chars)
  (looking-at (format "\\<\\(%s\\)\\>" regex)))

(defun hs-indent-previous-line-looking-at? (regex)
  "Are we looking at the regex on the previous line?"
  (save-excursion
    (let ((line (line-beginning-position)))
      (forward-line -1)
      (goto-char (line-beginning-position))
      (unless (= (point) line)
        (hs-indent-skip-non-word-chars)
        (looking-at (format "\\<\\(%s\\)\\>" regex))))))

(defun hs-indent-previous-line-contains? (regex)
  "Does the previous line contain this?"
  (save-excursion
    (let ((line (line-beginning-position)))
      (forward-line -1)
      (goto-char (line-beginning-position))
      (unless (= (point) line)
        (hs-indent-skip-non-word-chars)
        (search-forward-regexp 
         (format "\\(%s\\)" regex) 
         (line-end-position) t 1)))))

(defun hs-indent-change-level (column)
  "Set the indentation level for this line."
  (hs-indent-delete-line-indentation)
  (goto-char (line-beginning-position))
  (insert (make-string column ? )))

(defun hs-indent-delete-line-indentation ()
  (delete-region (line-beginning-position)
                 (+ (line-beginning-position) (hs-indent-level))))

(defun hs-indent-skip-non-word-chars ()
  "Skip none word characters on a line, like space, comma, brace, etc."
  (search-forward-regexp "[ {}(),]*" nil t))

(defun hs-indent-level ()
  "Get the indent level of the current line."
  (let ((end (save-excursion
               (goto-char (line-beginning-position))
               (search-forward-regexp "[^ ]" (line-end-position) t 1))))
    (if end
        (max 
         0 
         (1- (length
              (buffer-substring-no-properties
               (line-beginning-position)
               (or end (line-beginning-position))))))
      (- (line-end-position) (line-beginning-position)))))

(provide 'hs-indent)
