;;; hs.el — Haskell IDE for Emacs.

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

(require 'hs-align-imports)
(require 'hs-interactive-mode)
(require 'hs-cabal)
(require 'hs-cabal-mode)
(require 'hs-config)
(require 'hs-errors)
(require 'hs-faces)
(require 'hs-lang-en)
(require 'hs-process)
(require 'hs-project)
(require 'hs-sort-imports)
(require 'hs-navigate-imports)
(require 'hs-move-nested)
(require 'hs-completion)
(require 'hs-tags)
(require 'hs-types)
(require 'hs-mode)

(defun hs ()
  "Initialize everything necessary for correct functioning."
  (interactive)
  (unless (default-boundp '*hs-projects*)
    (setq *hs-projects* '()))
  (unless (default-boundp '*hs-project*)
    (setq *hs-project* nil)))

(provide 'hs)
