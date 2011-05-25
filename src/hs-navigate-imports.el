;; hs-navigate-imports.el — A function for cycling through Haskell import lists.
;; Copyright (C) 2010 Chris Done <chrisdone@gmail.com>

;; The cycling step will stop once at the last import list so
;; that it is easy to add a new import list.

;; This module works completely independently of any libraries
;; (including haskell-mode).

;; Exports three interactive functions:
;; 1. hs-navigate-imports
;; 2. hs-navigate-imports-go
;; 3. hs-navigate-imports-return

;; Example usage:

;; (require 'hs-navigate-imports)
;; (define-key haskell-mode-map [f8] 'hs-navigate-imports)

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(defvar hs-navigate-imports-start-point nil)

(defun hs-navigate-imports (&optional return)
  "Cycle the Haskell import lines or return to point (with prefix arg)."
  (interactive "P")
  (if return
      (hs-navigate-imports-return)
    (hs-navigate-imports-go)))

(defun hs-navigate-imports-go ()
  "Go to the first line of a list of consequtive import lines. Cycles."
  (interactive)
  (unless (or (hs-navigate-imports-line)
              (equal (line-beginning-position) (point-min))
              (save-excursion (previous-line)
                              (hs-navigate-imports-line)))
    (setq hs-navigate-imports-start-point (point)))
  (hs-navigate-imports-go-internal))

(defun hs-navigate-imports-go-internal ()
  "Go to the first line of a list of consequtive import lines. Cycle."
  (if (hs-navigate-imports-line)
      (progn (hs-navigate-imports-goto-end)
             (when (haskell-find-forward-import-line)
               (hs-navigate-imports-go-internal)))
    (let ((point (hs-navigate-imports-find-forward-line)))
      (if point
          (goto-char point)
        (progn (goto-char (point-min))
               (when (hs-navigate-imports-find-forward-line)
                 (hs-navigate-imports-go-internal)))))))

(defun hs-navigate-imports-return ()
  "Return to the non-import point we were at before going to the module list.
   If we were originally at an import list, we can just cycle through easily."
  (interactive)
  (when hs-navigate-imports-start-point
    (goto-char hs-navigate-imports-start-point)))

(defun hs-navigate-imports-goto-end ()
  "Skip a bunch of consequtive import lines."
  (while (not (or (equal (point)
                         (point-max))
                  (not (hs-navigate-imports-line))))
    (forward-line)))

(defun hs-navigate-imports-find-forward-line ()
  "Return a point with at an import line, or nothing."
  (save-excursion
    (while (not (or (equal (point) (point-max))
                    (hs-navigate-imports-after-imports-p) ;; This one just speeds it up.
                    (hs-navigate-imports-line)))
      (forward-line))
    (let ((point (point)))
      (if (hs-navigate-imports-line)
          (point)
        nil))))

(defun hs-navigate-imports-line ()
  "Try to match the current line as a regexp."
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (if (string-match "^import " line)
        line
      nil)))

(defun hs-navigate-imports-after-imports-p ()
  "Are we after the imports list? Just for a speed boost."
  (save-excursion
    (goto-char (line-beginning-position))
    (not (not (search-forward-regexp "\\( = \\|\\<instance\\>\\| :: \\)"
                                     (line-end-position) t 1)))))

(provide 'hs-navigate-imports)
