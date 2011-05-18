;;; hs-faces.el — Faces used throughout.

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

(require 'cl)

(defface hs-faces-ghci-prompt
  '((t :inherit 'font-lock-function-name-face))
  "Face for the prompt."
  :group 'hs)

(defface hs-faces-ghci-result
  '((t :inherit 'font-lock-string-face))
  "Face for the result."
  :group 'hs)

(defface hs-faces-type-result
  '((t :inherit 'font-lock-type-face))
  "Face for the result."
  :group 'hs)

(defface hs-faces-ghci-error
  '((t :inherit 'compilation-warning))
  "Face for error messages."
  :group 'hs)

(defface hs-faces-ghci-warning
  '((t :inherit 'compilation-warning))
  "Face for warning messages."
  :group 'hs)

(defface hs-faces-ghci-error '((t (:background "#000")))
  :group 'hs)

(provide 'hs-faces)
