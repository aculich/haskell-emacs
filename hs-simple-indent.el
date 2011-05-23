;;; hs-simple-indent.el -- Simple indentation module for Haskell Mode

;; Copyright (C) 1998 Heribert Schuetz, Graeme E Moss

;; Authors:
;;   1998 Heribert Schuetz <Heribert.Schuetz@informatik.uni-muenchen.de> and
;;        Graeme E Moss <gem@cs.york.ac.uk>
;; Keywords: indentation files Haskell
;; Version: 1.0
;; URL: http://www.cs.york.ac.uk/~gem/haskell-mode/simple-indent.html

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

;; (defun hs-simple-indent ()
;;   (interactive)
;;   (unless (= (point) (line-end-position))
;;     (let ((relative-col (current-column))
;;           (start-col (hs-move-nested-indent-level)))
;;       (goto-char (+ (line-beginning-position) start-col))
;;       (hs-simple-indent-line)
;;       (let* ((col (hs-move-nested-indent-level))
;;              (cols-moved (- col start-col)))
;;         (hs-move-nested cols-moved)
;;         (goto-char (+ (line-beginning-position)
;;                       relative-col
;;                       cols-moved))))))

(defun hs-simple-indent-line ()
  "Space out to under next visible indent point.
Indent points are positions of non-whitespace following whitespace in
lines preceeding point.  A position is visible if it is to the left of
the first non-whitespace of every nonblank line between the position and
the current line.  If there is no visible indent point beyond the current
column, `tab-to-tab-stop' is done instead."
  (interactive)
  (let* ((start-column (current-column))
         (invisible-from nil)		; `nil' means infinity here
         (indent
          (catch 'haskell-simple-indent-break
            (save-excursion
              (while (progn (beginning-of-line)
                            (not (bobp)))
                (forward-line -1)
                (if (not (looking-at "[ \t]*\n"))
                    (let ((this-indentation (current-indentation)))
                      (if (or (not invisible-from)
                              (< this-indentation invisible-from))
                          (if (> this-indentation start-column)
                              (setq invisible-from this-indentation)
                            (let ((end (line-beginning-position 2)))
                              (move-to-column start-column)
                              ;; Is start-column inside a tab on this line?
                              (if (> (current-column) start-column)
                                  (backward-char 1))
                              (or (looking-at "[ \t]")
                                  (skip-chars-forward "^ \t" end))
                              (skip-chars-forward " \t" end)
                              (let ((col (current-column)))
                                (throw 'haskell-simple-indent-break
                                       (if (or (= (point) end)
                                               (and invisible-from
                                                    (> col invisible-from)))
                                           invisible-from
                                         col)))))))))))))
    (if indent
	(let ((opoint (point-marker)))
	  (indent-line-to indent)
	  (if (> opoint (point))
	      (goto-char opoint))
	  (set-marker opoint nil))
      (tab-to-tab-stop))))
