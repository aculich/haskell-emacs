;;; hs-completion.el — Completion support (based on auto-complete mode).

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

(defvar hs-completion-ac-source
      '((candidates
         . (lambda ()
             (all-completions ac-target
                              (append nil
                                      hs-completion-ghc-extensions
                                      hs-completion-prelude
                                      hs-completion-prelude-types
                                      hs-completion-reserved-words))))))
(defun hs-completion ())

(provide 'hs-completion)