;;; hs-macros.el — Some macros used in this project. Might conflict.

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

;; So sue me.

(defmacro where (&rest body) 
  "Reverse of LET."
  `(let ,(reverse (car (last body))) 
     ,@(butlast body)))

(defmacro where* (&rest body) 
  "Reverse of LET* (declarations are reversed too)."
  `(let* ,(reverse (car (last body))) 
     ,@(butlast body)))

(provide 'hs-macros)