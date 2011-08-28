;;; hs-core.el --- Syntax highlighting module for GHC Core

;; Copyright (C) 2010  Johan Tibell

;; Author: Johan Tibell <johan.tibell@gmail.com>

;; Patched by Chris Done <chrisdone@gmail.com> (2011)

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Purpose:
;;
;; To make it easier to read GHC Core output by providing highlighting
;; and removal of commonly ignored annotations.

;;; Code:

(require 'hs-mode)

(defun hs-core-clean-region (start end)
  "Remove commonly ignored annotations and namespace
prefixes in the given region."
  (interactive "r")
  (save-restriction 
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward-regexp "GHC\.[^\.]*\." nil t)
      (replace-match "" nil t))
    (goto-char (point-min))
    (while (flush-lines "^ *GblId *$" nil))
    (goto-char (point-min))
    (while (flush-lines "^ *LclId *$" nil))
    (goto-char (point-min))
    (while (flush-lines (concat "^ *\\[\\(?:Arity [0-9]+\\|NoCafRefs\\|"
                                "Str: DmdType\\|Worker \\)"
                                "\\([^]]*\\n?\\).*\\] *$") nil))
    (goto-char (point-min))
    (while (search-forward "Main." nil t) (replace-match "" nil t))))

(defun hs-core-clean-buffer ()
  "Remove commonly ignored annotations and namespace
prefixes in the current buffer."
  (interactive)
  (hs-core-clean-region (point-min) (point-max)))

;;;###autoload
(defun hs-core-create-core ()
  "Compiled and load the current buffer as tidy core"
  (interactive)
  (save-buffer)
  (let ((core-buffer (generate-new-buffer "hs-core"))
        (neh (lambda () (kill-buffer core-buffer))))
    (add-hook 'next-error-hook neh)
    (call-process "ghc" nil core-buffer nil "-c" "-ddump-simpl" "-O2" (buffer-file-name))
    (display-buffer core-buffer)
    (with-current-buffer core-buffer
      (hs-core-mode))
    (remove-hook 'next-error-hook neh)))

;;;###autoload
(define-derived-mode hs-core-mode hs-mode "Haskell-Core"
  "Major mode for GHC Core files.")

(provide 'hs-core-mode)
;;; hs-core.el ends here