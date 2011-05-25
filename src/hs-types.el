;;; hs-types.el — All types in the project.

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

(defstruct
  (hs-project
   (:constructor hs-project-make))
  process
  name
  cabal-dir
  cabal-dev-dir
  prompt-history)

(defstruct
  (hs-process
   (:constructor hs-process-make))
  process
  cmd
  name
  response
  response-cursor
  load-dirs
  current-dir
  response-callback)

(defun hs-types ())

(provide 'hs-types)