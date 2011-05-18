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

(define-key hs-interactive-mode-map (kbd "RET")
  '(lambda ()
     (interactive)
     (hs-buffer-handle-ret *hs-project*)))

;; (define-key hs-mode-map (kbd "M-p")
;;   '(lambda ()
;;      (interactive)
;;      (hs-prompt-history-cycle *hs-session* 'left)))
;; (define-key hs-mode-map (kbd "M-n")
;;   '(lambda ()
;;      (interactive)
;;      (hs-prompt-history-cycle *hs-session* 'right)))
;; (define-key hs-mode-map (kbd "C-a")
;;   (lambda () (interactive)
;;     (hs-prompt-goto-start *hs-session*)))
;; (define-key hs-mode-map (kbd "\C-c\C-t")
;;   'hs-editor-type-at-point)

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

(defun hs-buffer-echo-read-only (project message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (hs-buffer project)
    (save-excursion
      (hs-buffer-goto-end-point)
      (insert (propertize (concat "\n" message)
                          'read-only t
                          'rear-nonsticky t)))))

(defun hs-buffer-echo-type (project message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (hs-buffer project)
    (save-excursion
      (hs-buffer-goto-end-point)
      (insert (propertize (concat "\n" message)
                          'read-only t
                          'face 'hs-faces-type-result
                          'rear-nonsticky t)))))

(defun hs-buffer-echo-read-only-incomplete (project message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (hs-buffer project)
    (save-excursion
      (hs-buffer-goto-end-point)
      (insert (propertize message
                          'face 'hs-faces-ghci-result
                          'read-only t
                          'rear-nonsticky t
                          'result t)))))

(defun hs-buffer-echo-error (project message)
  "Echo an error message before the prompt."
  (with-current-buffer (hs-buffer project)
    (save-excursion
      (hs-buffer-goto-end-point)
      (insert "\n")
      (insert (propertize message
                          'face 'hs-faces-ghci-error
                          'read-only t
                          'rear-nonsticky t
                          'error t)))))

(defun hs-buffer-echo-warning (project message)
  "Echo a warning message."
  (with-current-buffer (hs-buffer project)
    (save-excursion
      (hs-buffer-goto-end-point)
      (insert "\n")
      (insert (propertize message
                          'face 'hs-faces-ghci-warning
                          'read-only t
                          'rear-nonsticky t
                          'warning t)))))

(defun hs-buffer-handle-ret (project)
  "Handle the RET key in the buffer."
  (interactive)
  (with-current-buffer (hs-buffer project)
    (if (save-excursion (search-backward-regexp hs-config-buffer-prompt
                                                (line-beginning-position)
                                                t
                                                1))
        (hs-buffer-handle project)
      ;; This is a cheap solution. Better is to highlight all lines
      ;; with problems in the buffers themselves; have an in-memory
      ;; mapping of latest errors.
      (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                  (line-end-position))))
        (if (string-match "^[^:]+: \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" line)
            (let ((file (match-string 1 line))
                  (line (match-string 2 line))
                  (col (match-string 3 line)))
              (let* ((cabal-path (hs-project-cabal-dir project))
                     (src-path (hs-process-current-dir (hs-project-process project)))
                     (cabal-relative-file (concat cabal-path "/" file))
                     (src-relative-file (concat src-path "/" file)))
                (let ((file (cond ((file-exists-p cabal-relative-file)
                                   cabal-relative-file)
                                  ((file-exists-p src-relative-file) 
                                   src-relative-file))))
                  (when file
                    (other-window 1)
                    (find-file file)
                    (goto-line (string-to-number line))
                    (goto-char (+ (point) (string-to-number col))))))))))))

(defun hs-buffer-handle (project)
  "Take input from the current prompt."
  (let ((input
         (substring (buffer-substring-no-properties
                     (save-excursion
                       (goto-char (point-max))
                       (search-backward-regexp hs-config-buffer-prompt))
                     (point-max))
                    (length hs-config-buffer-prompt))))
    ;;    (hs-buffer-add-to-history project input)
    (hs-process-eval project
                     (replace-regexp-in-string
                      "\n"
                      " "
                      input))))

(defun hs-buffer-eval-insert-result (project result)
  "Insert the result of an eval."
  (with-current-buffer (hs-buffer project)
    (goto-char (point-max))
    (insert "\n")
    (insert (propertize result
                        'face 'hs-faces-ghci-result
                        'read-only t
                        'rear-nonsticky t
                        'prompt t
                        'result t))))

(provide 'hs-buffer)
