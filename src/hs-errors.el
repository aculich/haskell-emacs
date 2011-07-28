;;; hs-errors.el — Error/warning reformatting.

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

(defun hs-errors ())

(defun hs-errors-message-type (msg warning)
  "One-word description of the type of message."
  (cond ((hs-errors-unused-p msg) (hs-lang-error-unused))
        ((hs-errors-missing-sig-p msg) (hs-lang-error-missing-signature))
        ((hs-errors-defaulting-p msg) (hs-lang-errors-defaulting))
        ((hs-errors-type-p msg) (hs-lang-errors-mismatch))
        ((hs-errors-ambiguous-p msg) (hs-lang-errors-ambiguous))
        ((hs-errors-illegal-instance msg) (hs-lang-errors-illegal))
        ((hs-errors-no-instance-p msg) (hs-lang-errors-no-instance))
        ((hs-errors-cant-deduce-p msg) (hs-lang-errors-could-not-deduce))
        (warning (hs-lang-errors-warning))
        (t (hs-lang-errors-error))))

(defun hs-errors-line (error-msg line)
  (cond ((hs-errors-func-dep-mismatch error-msg)
         (string-to-number (match-string 4 error-msg)))
        (t line)))

(defun hs-errors-reduce-error-msg (line)
  "Remove any redundancy in error/warning messages."
  (cond ((or (hs-errors-unused-p line)
             (hs-errors-missing-sig-p line)
             (hs-errors-defaulting-p line))
         (format "%s" (match-string 1 line)))
        ((hs-errors-illegal-instance line)
         (format "%s" (match-string 1 line)))
        ((hs-errors-type-p line)
         (hs-lang-errors-x-against-y
          (match-string 1 line)
          (match-string 2 line)))
        ((or (hs-errors-cant-deduce-p line)
             (hs-errors-no-instance-p line))
         (hs-lang-errors-x-in-y
          (match-string 1 line)
          (match-string 2 line)))
        ((hs-errors-ambiguous-p line)
         (hs-lang-errors-x-in-y
          (match-string 2 line)
          (match-string 3 line)))
        ((hs-errors-incomplete-do-p line)
         (hs-lang-errors-incomplete-do))
        ((hs-errors-func-dep-mismatch line)
         (format "Use of %s: “%s” against “%s”"
                 (match-string 3 line)
                 (replace-regexp-in-string "[ \t\r\n]+" " " (match-string 1 line))
                 (replace-regexp-in-string "[ \t\r\n]+" " " (match-string 2 line))))
        (t line)))

(defun hs-errors-func-dep-mismatch (line)
  "Mismatch when using functional dependencies?"
  (string-match "^Couldn't match expected type `\\(.+?\\)'[ \n\t\r]+against inferred type `\\([[:unibyte:]]+?\\)'[\t\n\r ]+When using functional dependencies to combine[[:unibyte:]]+?arising from a use of `\\(.+?\\)'[\n\t\r ]+at [^:]+:\\([0-9]+\\)" line))

(defun hs-errors-illegal-instance (line)
  "Illegal instance declaration?"
  (string-match
   "^Illegal instance declaration for `\\(.+?\\)'"
   line))

(progn (when  (format "%s:%s: “%s” against “%s”" (match-string 3 test-msg) (match-string 4 test-msg) (match-string 1 test-msg) (match-string 2 test-msg))))

(defun hs-errors-cant-deduce-p (line)
  "Is it a deduction error?"
  (string-match "^Could not deduce (\\(.+?\\))\n[ ]+from the context (\\(.+?\\))\n" line))

(defun hs-errors-incomplete-do-p (line)
  "Is it a missing expression in a do?"
  (string-match "^The last statement in a 'do' construct must be an expression$"
                line))

(defun hs-errors-missing-sig-p (line)
  "Missing signature on a definition?"
  (string-match "^Definition but no type signature for `\\(.+?\\)'" line))

(defun hs-errors-defaulting-p (line)
  "Defaulting constraint?"
  (string-match "^Defaulting the following constraint(s) to type `\\(.+?\\)'"
                line))

(defun hs-errors-illegal-instance (line)
  "Illegal instance declaration?"
  (string-match
   "^Illegal instance declaration for `\\(.+?\\)'"
   line))

(defun hs-errors-ambiguous-p (line)
  "Ambiguous type variable?"
  (string-match
   (concat "^Ambiguous type variable `\\(.+?\\)' in the constraint:[\n ]+`\\(.+?\\)'"
           "[\n ]+arising from a use of `\\(.+?\\)'")
   line))

(defun hs-errors-no-instance-p (line)
  "Ambiguous type variable?"
  (string-match
   (concat "No instance for (\\(.+?\\))\n[ ]+"
           "[\n ]+arising from the literal `\\(.+?\\)'")
   line))

(defun hs-errors-type-p (line)
  "Type error?"
  (string-match
   "^Couldn't match expected type `\\(.+\\)'[\n ]+against inferred type `\\(.+?\\)'"
   line))

(defun hs-errors-unused-p (line)
  "Redundant import / unused variable?"
  (or (string-match "^The import of `\\(.+\\)' is redundant$" line)
      (string-match "^Defined but not used: `\\(.+\\)'$" line)))

(provide 'hs-errors)
