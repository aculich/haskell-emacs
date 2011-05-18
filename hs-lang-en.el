;;; hs-lang-en.el — Provides English language strings.

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

(defun hs-lang-en ())

(defun hs-lang-new-project-name ()
  "New project name: ")

(defun hs-lang-choose-project ()
  "Choose project: ")

(defun hs-lang-create-new-project (name)
  (format "Create new project “%s”? " name))

(provide 'hs-lang-en)
