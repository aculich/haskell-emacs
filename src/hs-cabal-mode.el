;;; hs-cabal-mode.el --- Support for Cabal packages

;; Copyright (C) 2007, 2008  Stefan Monnier

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;; Todo:

;; - distinguish continued lines from indented lines.
;; - indent-line-function.
;; - outline-minor-mode.

;;; Code:

(require 'cl)

(defconst hs-cabal-mode-general-fields
  ;; Extracted with (haskell-cabal-extract-fields-from-doc "general-fields")
  '("name" "version" "cabal-version" "license" "license-file" "copyright"
    "author" "maintainer" "stability" "homepage" "package-url" "synopsis"
    "description" "category" "tested-with" "build-depends" "data-files"
    "extra-source-files" "extra-tmp-files"))

(defconst hs-cabal-mode-library-fields
  ;; Extracted with (hs-cabal-mode-extract-fields-from-doc "library")
  '("exposed-modules"))

(defconst hs-cabal-mode-executable-fields
  ;; Extracted with (hs-cabal-mode-extract-fields-from-doc "executable")
  '("executable" "main-is"))

(defconst hs-cabal-mode-buildinfo-fields
  ;; Extracted with (hs-cabal-mode-extract-fields-from-doc "buildinfo")
  '("buildable" "other-modules" "hs-source-dirs" "extensions" "ghc-options"
    "ghc-prof-options" "hugs-options" "nhc-options" "includes"
    "install-includes" "include-dirs" "c-sources" "extra-libraries"
    "extra-lib-dirs" "cc-options" "ld-options" "frameworks"))

(defvar hs-cabal-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; The comment syntax can't be described simply in syntax-table.
    ;; We could use font-lock-syntactic-keywords, but is it worth it?
    ;; (modify-syntax-entry ?-  ". 12" st)
    (modify-syntax-entry ?\n ">" st)
    st))

(defvar hs-cabal-mode-font-lock-keywords
  ;; The comment syntax can't be described simply in syntax-table.
  ;; We could use font-lock-syntactic-keywords, but is it worth it?
  '(("^[ \t]*--.*" . font-lock-comment-face)
    ("^ *\\([^ \t:]+\\):" (1 font-lock-keyword-face))
    ("^\\(Library\\)[ \t]*\\({\\|$\\)" (1 font-lock-keyword-face))
    ("^\\(Executable\\)[ \t]+\\([^\n \t]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))
    ("^\\(Flag\\)[ \t]+\\([^\n \t]*\\)"
     (1 font-lock-keyword-face) (2 font-lock-constant-face))
    ("^ *\\(if\\)[ \t]+.*\\({\\|$\\)" (1 font-lock-keyword-face))
    ("^ *\\(}[ \t]*\\)?\\(else\\)[ \t]*\\({\\|$\\)"
     (2 font-lock-keyword-face))))

(defvar hs-cabal-mode-buffers nil
  "List of Cabal buffers.")

;; (defsubst* inferior-haskell-string-prefix-p (str1 str2)
;;   "Return non-nil if STR1 is a prefix of STR2"
;;   (eq t (compare-strings str2 nil (length str1) str1 nil nil)))

(autoload 'derived-mode-p "derived")	; Emacs 21

(defun hs-cabal-mode-buffers-clean (&optional buffer)
  (let ((bufs ()))
    (dolist (buf hs-cabal-mode-buffers)
      (if (and (buffer-live-p buf) (not (eq buf buffer))
               (with-current-buffer buf (derived-mode-p 'hs-cabal-mode)))
          (push buf bufs)))
    (setq hs-cabal-mode-buffers bufs)))

(defun hs-cabal-mode-unregister-buffer ()
  (hs-cabal-mode-buffers-clean (current-buffer)))

;;;###autoload
(define-derived-mode hs-cabal-mode fundamental-mode "Cabal-Config"
  "Major mode for Cabal package description files."
  (set (make-local-variable 'font-lock-defaults)
       '(hs-cabal-mode-font-lock-keywords t t nil nil))
  (add-to-list 'hs-cabal-mode-buffers (current-buffer))
  (add-hook 'change-major-mode-hook 'hs-cabal-mode-unregister-buffer nil 'local)
  (add-hook 'kill-buffer-hook 'hs-cabal-mode-unregister-buffer nil 'local)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'comment-start-skip) "\\(^[ \t]*\\)--[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-end-skip) "[ 	]*\\(\\s>\\|\n\\)"))

(defun hs-cabal-mode-get-setting (name)
  (save-excursion
    (let ((case-fold-search t))
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^" (regexp-quote name)
                     ":[ \t]*\\(.*\\(\n[ \t]+[ \t\n].*\\)*\\)")
             nil t)
        (let ((val (match-string 1))
              (start 1))
          (when (match-end 2)             ;Multiple lines.
            ;; The documentation is not very precise about what to do about
            ;; the \n and the indentation: are they part of the value or
            ;; the encoding?  I take the point of view that \n is part of
            ;; the value (so that values can span multiple lines as well),
            ;; and that only the first char in the indentation is part of
            ;; the encoding, the rest is part of the value (otherwise, lines
            ;; in the value cannot start with spaces or tabs).
            (while (string-match "^[ \t]\\(?:\\.$\\)?" val start)
              (setq start (1+ (match-beginning 0)))
              (setq val (replace-match "" t t val))))
          val)))))

(provide 'hs-cabal-mode)

;;; hs-cabal-mode.el ends here
