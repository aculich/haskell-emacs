;;; hs-config.el — Elisp configurable items.

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

(defun hs-config ())

(defvar hs-config-default-project-name "haskell")

(defvar hs-config-cabal-dev-bin "/home/chris/.cabal/bin/cabal-dev")

(defvar hs-config-ghci-bin "ghci")

(defvar hs-config-process-prompt-regex "\\(^> $\\|\n[> ]*> $\\)")

(defvar hs-config-buffer-prompt "λ> ")

(defvar hs-config-tags-cmd ":!find . -name '*.hs' | xargs hasktags -e -x")

(defvar hs-config-scripts '())

(defvar hs-align-imports-regex
  (concat "^\\(import[ ]+\\)"
          "\\(qualified \\)?"
          "[ ]*\\(\"[^\"]*\" \\)?"
          "[ ]*\\([A-Za-z0-9_.']*\\)"
          "[ ]*\\([ ]*as [A-Z][^ ]*\\)?"
          "[ ]*\\((.*)\\)?"
          "\\([ ]*hiding (.*)\\)?"
          "\\( -- .*\\)?[ ]*$"))

(provide 'hs-config)
