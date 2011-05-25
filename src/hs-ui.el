;;; hs-process.el — UI procedures.

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
(require 'hs-string)

(defun hs-ui ())

(defun hs-message-line (str)
  "Message only one line, multiple lines just disturbs the programmer."
  (let ((lines (split-string str "\n" t)))
    (when (and (car lines) (stringp (car lines)))
      (message (concat (car lines)
                       (if (and (cdr lines) (stringp (cadr lines)))
                           (format " [ %s … ]" (hs-string-take (hs-trim (cadr lines)) 10))
                           ""))))))

(provide 'hs-ui)