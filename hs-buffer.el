;;; hs-buffer.el — Interactive buffer functions.

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

(require 'cl)

(define-derived-mode hs-interactive-mode nil "Interactive Haskell" ""
  (make-local-variable 'hs-interactive-mode)
  (setq hs-interactive-mode t))

(defvar hs-interactive-mode
  (let ((map (make-sparse-keymap)))
    map)
  "Interactive Haskell mode map.")

(defun hs-buffer-create (project)
  "Make an interactive Haskell buffer."
  (get-buffer-create (hs-buffer-name project))
  (with-current-buffer (hs-buffer project)
    (kill-all-local-variables)
    (use-local-map hs-interactive-mode-map)
    (setq major-mode 'hs-interactive-mode)
    (setq mode-name "Interactive Haskell")
    (run-mode-hooks 'hs-interactive-mode-hook)
    (hs-buffer-prompt project)
    (hs-buffer-welcome-message project)
    (hs-project-choose project)))

(defun hs-buffer (project)
  "Get the current buffer."
  (get-buffer (hs-buffer-name project)))

(defun hs-buffer-name (project)
  "Name the buffer based on project name."
  (concat "*" (hs-project-name project) "*"))

(defun hs-buffer-welcome-message (project)
  "Echo the welcome message."
  (hs-buffer-echo-read-only project (hs-lang-welcome-message)))

(defun hs-buffer-goto-end-point ()
  "Go to the 'end' of the buffer (before the prompt.)"
  (goto-char (point-max))
  (search-backward-regexp hs-config-buffer-prompt)
  (backward-char))

(defun hs-buffer-prompt (project)
  "Show a prompt at the end of the buffer."
  (with-current-buffer (hs-buffer project)
    (goto-char (point-max))
    (insert "\n")
    (insert (propertize hs-config-buffer-prompt
                        'face 'hs-faces-ghci-prompt
                        'read-only t
                        'rear-nonsticky t
                        'prompt t))))

(defun hs-buffer-echo-read-only (session message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (hs-buffer session)
    (save-excursion
      (hs-buffer-goto-end-point)
      (insert (propertize (concat "\n" message)
                          'read-only t
                          'rear-nonsticky t)))))

(defun hs-buffer-echo-type (session message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (hs-buffer session)
    (save-excursion
      (hs-buffer-goto-end-point)
      (insert (propertize (concat "\n" message)
                          'read-only t
                          'face 'hs-faces-type-result
                          'rear-nonsticky t)))))

(defun hs-buffer-echo-read-only-incomplete (session message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (hs-buffer session)
    (save-excursion
      (hs-buffer-goto-end-point)
      (insert (propertize message
                          'face 'hs-faces-ghci-result
                          'read-only t
                          'rear-nonsticky t
                          'result t)))))

(defun hs-buffer-echo-error (session message)
  "Echo an error message before the prompt."
  (with-current-buffer (hs-buffer session)
    (save-excursion
      (hs-buffer-goto-end-point)
      (insert "\n")
      (insert (propertize message
                          'face 'hs-faces-ghci-error
                          'read-only t
                          'rear-nonsticky t
                          'error t)))))

(defun hs-buffer-echo-warning (session message)
  "Echo a warning message."
  (with-current-buffer (hs-buffer session)
    (save-excursion
      (hs-buffer-goto-end-point)
      (insert "\n")
      (insert (propertize message
                          'face 'hs-faces-ghci-warning
                          'read-only t
                          'rear-nonsticky t
                          'warning t)))))

(provide 'hs-buffer)
