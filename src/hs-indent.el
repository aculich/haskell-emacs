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

(defvar hs-indent-keyword-indent-list
  '(("case" . 2)
    ("let" . 4)))

(defun hs-indent (change)
  "Indent line(s) depending on what's above and below and where the point is at."
  (or (hs-indent-bring-backs-direct)
      (hs-indent-bring-backs-roots)
      (hs-indent-data)
      (hs-indent-do)
      (hs-indent-keywords)
      (hs-indent-function-decl)
      (hs-indent-case-cond)
      (hs-indent-default-bring-in)
      (hs-indent-default-bring-back)))

(defun hs-indent-do ()
  "Indent for do blocks."
  )

(defun hs-indent-case-cond ()
  "Indent when inside a case condition."
  nil)

(defun hs-indent-bring-backs-direct ()
  "Bring back the indentation if there shouldn't be any indentation on this line."
  (where* 
   (when bring-back?
     (hs-indent-delete-line-indentation)
     t)
   ((bring-back? (hs-indent-previous-line-looking-at? choices))
    (choices (mapconcat 'identity hs-indent-bring-back-list "\\|")))))

(defun hs-indent-bring-backs-roots ()
  "Bring back the indentation if there shouldn't be any indentation on this line."
  (where* 
   (when bring-back?
     (hs-indent-delete-line-indentation)
     t)
   ((bring-back? (hs-indent-root-looking-at? choices))
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

(defun hs-indent-keywords ()
  "Indent specific keywords."
  (or (hs-indent-keywords-immediate)
      (hs-indent-keywords-sibling)))

(defun hs-indent-keywords-immediate ()
  "Indent keywords that are on the previous line."
  (where*
   (when previous-keyword
     (hs-indent-relative/previous-line
      (or (cdr previous-keyword)
          2))
     t)
   ((previous-keyword
     (find-if (lambda (kw)
                (hs-indent-previous-line-looking-at? (car kw)))
              hs-indent-keyword-indent-list)))))

(defun hs-indent-keywords-sibling ()
  "Indent by keywords that are siblings."
  (where*
   (when previous-keyword
     (hs-indent-relative (or (cdr previous-keyword)
                             2))
     t)
   ((previous-keyword
     (find-if (lambda (kw)
                (hs-indent-sibling-looking-at? (car kw)))
              hs-indent-keyword-indent-list)))))

(defun hs-indent-relative/previous-line (col)
  "Indent relative to the previous line's indent level."
  (hs-indent-change-level (+ (hs-indent-previous-line-level)
                             col)))

(defun hs-indent-function-decl ()
  "Indent after function declaration."
  (when (hs-indent-previous-line-contains? "=")
    (hs-indent-change-level 2)
    t))

(defun hs-indent-root-looking-at? (word)
  (save-excursion (hs-indent-goto-root)
                  (hs-indent-looking-at? word)))

(defun hs-indent-sibling-looking-at? (word)
  (save-excursion (hs-indent-goto-sibling)
                  (hs-indent-looking-at? word)))

(defun hs-indent-goto-root ()
  "Go to the parent line (one column backwards)."
  (hs-indent-goto-previous-level 0))

(defun hs-indent-goto-parent ()
  "Go to the parent line (one column backwards)."
  (hs-indent-goto-previous-level (- (hs-indent-level) 2)))

(defun hs-indent-goto-sibling ()
  "Go to the previous sibling line (same column)."
  (hs-indent-goto-previous-level (hs-indent-level)))

(defun hs-indent-goto-previous-level (level)
  "Go to the first line and column at a given indentation level."
  (where*
   (when point
     (goto-char (+ point level)))
   ((point (save-excursion
             (goto-char (line-beginning-position))
             (search-backward-regexp
              (format "\\(^%s[^ ]\\|^%s$\\)"
                      spaces spaces)
              nil t 1)))
    (spaces (make-string level ? )))))

(defun hs-indent-default-bring-in ()
  "If we don't know what to do, but we're on a blank line, indent."
  (when (and (hs-indent-blank-line?)
             (> (hs-indent-previous-line-level) 0))
    (hs-indent-relative 2)
    t))

(defun hs-indent-blank-line? ()
  "Are we on a blank line?"
  (not (eq nil
           (string-match "^[ ]*$" (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))))

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

(defun hs-indent-relative (change)
  "Set the indentation level for this line."
  (hs-indent-change-level (+ (hs-indent-level) change)))

(defun hs-indent-delete-line-indentation ()
  (delete-region (line-beginning-position)
                 (+ (line-beginning-position) (hs-indent-level))))

(defun hs-indent-skip-non-word-chars ()
  "Skip none word characters on a line, like space, comma, brace, etc."
  (search-forward-regexp "[ {}(),]*" nil t))

(defun hs-indent-previous-line-level ()
  "Get the indent level of the previous line"
  (save-excursion (forward-line -1)
                  (hs-indent-level)))

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
