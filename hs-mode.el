;;; hs-mode.el — Haskell editing mode.

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

(require 'hs-types)

(define-derived-mode hs-mode nil "Haskell" ""
  (kill-all-local-variables)
  (make-local-variable 'hs-mode)
  (setq hs-mode t)
  (use-local-map hs-mode-map)
  (setq major-mode 'hs-mode)
  (setq mode-name "Haskell")
  (set (make-local-variable 'font-lock-defaults)
       '(hs-mode-font-lock-keywords t nil nil nil)))

(defvar hs-mode-font-lock-keywords
  `(;; Comments
    ("[ ]+-- .*" . font-lock-comment-face)
    ("^-- | .*" . font-lock-doc-face)
    ("^--.*" . font-lock-comment-face)
    ;; Strings
    (,(concat "\\(\\(\"\\|\n[ \t]*\\\\\\)\\([^\"\\\\\n]\\|\\\\.\\)"
              "*\\(\"\\|\\\\[ \t]*$\\)\\|'\\([^'\\\\\n]\\|\\\\.[^'\n]*\\)'\\)")
     . font-lock-string-face)
    ;; Keywords
    (,(format 
       "\\<%s\\>"
       (regexp-opt '("case" "class" "data" "default" "deriving" "do" "else"
                     "if" "import" "in" "infix" "infixl" "infixr" "instance" 
                     "let" "module" "newtype" "of" "then" "type" "where" "as"
                     "qualified" "hiding")))
     . font-lock-keyword-face)
    ;; Constructors and types
    ("\\(\\<[A-Z][A-Za-z0-9_']*\\>\\|\\[\\]\\|()\\)" . font-lock-type-face)
    ;; Function declarations
    ("^[_a-z][A-Za-z0-9_']*" . font-lock-function-name-face)
    ;; Numbers
    ("[0-9]+\\.?[0-9]*" . font-lock-constant-face)
    ;; Operators
    ("[-!#$%&\\*\\+\\./<=>\\?@\\\\^\\|~]+" . font-lock-operator-face)
    ;; Reserved symbols
    (,(regexp-opt '(".." "::" "=" "\\" "|" "<-" "->"
                    "@" "~" "=>") t)
     . font-lock-keyword-face)))

(defvar hs-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Haskell mode map.")

(provide 'hs-mode)
