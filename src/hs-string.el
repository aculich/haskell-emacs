;;; hs-string.el — String functions.

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

(defun hs-string ())

(defun hs-trim (string)
  (replace-regexp-in-string
   "^[ \t\n]+" ""
   (replace-regexp-in-string
    "[ \t\n]+$" ""
    string)))

(defun hs-string-take (string n)
  "Take n chars from string."
  (substring string
             0
             (min (length string) n)))

(defun hs-is-prefix-of (x y)
  "Is x string a prefix of y string?"
  (string= (substring x 0 (min (length x) (length y)))
           (substring y 0 (min (length x) (length y)))))

(provide 'hs-string)