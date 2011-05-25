;;; hs-tags.el — TAGS-based features.

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

(require 'hs-lang-en)
(require 'hs-types)

(require 'cl)

(defun hs-tags ())

(defun hs-tags-generate-interactive ()
  "Send a (silent; don't tell me about it in the REPL) arbitrary command."
  (interactive)
  (let ((project (hs-project)))
   (setf (hs-process-cmd (hs-project-process project)) 'tags-generate)
   (process-send-string (hs-process-process (hs-project-process project))
                        (concat hs-config-tags-cmd "\n"))))

(provide 'hs-tags)