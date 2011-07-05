;;; hs-interactive-mode.el — Interactive buffer functions.

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

(defvar hs-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'hs-interactive-mode-handle-ret-interactive)
    (define-key map (kbd "C-j") 'hs-interactive-mode-newline-indent)
    (define-key map (kbd "C-a") 'hs-interactive-mode-handle-start-interactive)
    (define-key map (kbd "C-c C-k") 'hs-interactive-mode-clear)
    (define-key map (kbd "M-p")
      '(lambda () (interactive) (hs-interactive-mode-history-toggle -1)))
    (define-key map (kbd "M-n")
      '(lambda () (interactive) (hs-interactive-mode-history-toggle 1)))
    map)
  "Interactive Haskell mode map.")

(define-derived-mode hs-interactive-mode nil "Interactive-Haskell" ""
  (kill-all-local-variables)
  (make-local-variable 'hs-interactive-mode)
  (setq hs-interactive-mode t)
  (setq major-mode 'hs-interactive-mode)
  (setq mode-name "Interactive-Haskell")
  (run-mode-hooks 'hs-interactive-mode-hook)
  (hs-interactive-mode-prompt project)
  (hs-interactive-mode-welcome-message project)
  (hs-project-choose project)
  (use-local-map hs-interactive-mode-map)
  (make-local-variable 'hs-interactive-mode-history)
  (make-local-variable 'hs-interactive-mode-history-index)
  (setq hs-interactive-mode-history '())
  (setq hs-interactive-mode-history-index 0))

(defun hs-interactive-mode-handle-start-interactive ()
  (interactive)
  (with-current-buffer (hs-interactive-mode-buffer (hs-project))
    (if (search-backward-regexp hs-config-buffer-prompt
                                (line-beginning-position)
                                t
                                1)
        (search-forward-regexp hs-config-buffer-prompt
                               (line-end-position)
                               t
                               1)
      (move-beginning-of-line nil))))

(defun hs-interactive-mode-handle-ret-interactive ()
  (interactive)
  (hs-interactive-mode-handle-ret (hs-project)))

(defun hs-interactive-mode-create (project)
  "Make an interactive Haskell buffer."
  (get-buffer-create (hs-interactive-mode-name project))
  (with-current-buffer (hs-interactive-mode-buffer project)
    (hs-interactive-mode)))

(defun hs-interactive-mode-buffer (project)
  "Get the current buffer."
  (get-buffer (hs-interactive-mode-name project)))

(defun hs-interactive-mode-name (project)
  "Name the buffer based on project name."
  (concat "*" (hs-project-name project) "*"))

(defun hs-interactive-mode-welcome-message (project)
  "Echo the welcome message."
  (hs-interactive-mode-echo-read-only project (hs-lang-welcome-message)))

(defun hs-interactive-mode-goto-end-point ()
  "Go to the 'end' of the buffer (before the prompt.)"
  (goto-char (point-max))
  (search-backward-regexp hs-config-buffer-prompt)
  (backward-char))

(defun hs-interactive-mode-prompt (project)
  "Show a prompt at the end of the buffer."
  (with-current-buffer (hs-interactive-mode-buffer project)
    (goto-char (point-max))
    (insert "\n")
    (insert (propertize hs-config-buffer-prompt
                        'face 'hs-faces-ghci-prompt
                        'read-only t
                        'rear-nonsticky t
                        'prompt t))))

(defun hs-interactive-mode-echo-read-only (project message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (hs-interactive-mode-buffer project)
    (save-excursion
      (hs-interactive-mode-goto-end-point)
      (insert (propertize (concat "\n" message)
                          'read-only t
                          'rear-nonsticky t)))))

(defun hs-interactive-mode-echo-type (project message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (hs-interactive-mode-buffer project)
    (save-excursion
      (hs-interactive-mode-goto-end-point)
      (insert (propertize (concat "\n" message)
                          'read-only t
                          'face 'hs-faces-type-result
                          'rear-nonsticky t)))))

(defun hs-interactive-mode-echo-read-only-incomplete (project message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (hs-interactive-mode-buffer project)
    (save-excursion
      (hs-interactive-mode-goto-end-point)
      (insert (propertize message
                          'face 'hs-faces-ghci-result
                          'read-only t
                          'rear-nonsticky t
                          'result t)))))

(defun hs-interactive-mode-echo-error (project message)
  "Echo an error message before the prompt."
  (with-current-buffer (hs-interactive-mode-buffer project)
    (save-excursion
      (hs-interactive-mode-goto-end-point)
      (insert "\n")
      (insert (propertize message
                          'face 'hs-faces-ghci-error
                          'read-only t
                          'rear-nonsticky t
                          'error t)))))

(defun hs-interactive-mode-echo-warning (project message)
  "Echo a warning message."
  (with-current-buffer (hs-interactive-mode-buffer project)
    (save-excursion
      (hs-interactive-mode-goto-end-point)
      (insert "\n")
      (insert (propertize message
                          'face 'hs-faces-ghci-warning
                          'read-only t
                          'rear-nonsticky t
                          'warning t)))))

(defun hs-interactive-mode-handle-ret (project)
  "Handle the RET key in the buffer."
  (interactive)
  (let ((line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (with-current-buffer (hs-interactive-mode-buffer project)
      (if (and (not (string-match "^[^:]+: \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):" line))
               (save-excursion (search-backward-regexp hs-config-buffer-prompt
                                                       nil
                                                       t
                                                       1)))
          (hs-interactive-mode-handle project)
        ;; This is a cheap solution. Better is to highlight all lines
        ;; with problems in the buffers themselves; have an in-memory
        ;; mapping of latest errors.

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
                    (goto-char (point-max))
                    (other-window 1)
                    (find-file file)
                    (goto-line (string-to-number line))
                    (goto-char (+ (point) (string-to-number col))))))))))))

(defun hs-interactive-mode-input (project)
  (substring (buffer-substring-no-properties
              (save-excursion
                (goto-char (point-max))
                (search-backward-regexp hs-config-buffer-prompt))
              (point-max))
             (length hs-config-buffer-prompt)))

(defun hs-interactive-mode-handle (project)
  "Take input from the current prompt."
  (let ((input (hs-interactive-mode-input project)))
    (unless (string= input "")
      (hs-interactive-mode-history-add input)
      (hs-process-eval project
                       (replace-regexp-in-string
                        "\n"
                        " "
                        input)))))

(defun hs-interactive-mode-eval-insert-result (project result)
  "Insert the result of an eval."
  (with-current-buffer (hs-interactive-mode-buffer project)
    (goto-char (point-max))
    (insert "\n")
    (insert (propertize result
                        'face 'hs-faces-ghci-result
                        'read-only t
                        'rear-nonsticky t
                        'prompt t
                        'result t))))

(defun hs-interactive-mode-history-toggle (n)
  "Toggle the history n items up or down."
  (unless (null hs-interactive-mode-history)
    (hs-interactive-mode-set-prompt
     (nth hs-interactive-mode-history-index
          hs-interactive-mode-history))
    (setq hs-interactive-mode-history-index
          (mod (+ n hs-interactive-mode-history-index)
               (length hs-interactive-mode-history)))))

(defun hs-interactive-mode-history-add (input)
  "Add item to the history."
  (setq hs-interactive-mode-history
        (cons input
              (remove-if (lambda (i) (string= i input))
                         hs-interactive-mode-history)))
  (setq hs-interactive-mode-history-index 0))

(defun hs-interactive-mode-set-prompt (p)
  "Set (and overwrite) the current prompt."
  (with-current-buffer (hs-interactive-mode-buffer (hs-project))
    (goto-char (point-max))
    (goto-char (line-beginning-position))
    (search-forward-regexp hs-config-buffer-prompt)
    (delete-region (point)
                   (line-end-position))
    (insert p)))

(defun hs-interactive-mode-newline-indent ()
  "Newline and indent at the prompt."
  (interactive)
  (insert "\n   "))

(defun hs-interactive-mode-clear ()
  "Newline and indent at the prompt."
  (interactive)
  (let ((inhibit-read-only t))
    (set-text-properties (point-min) (point-max) nil))
  (delete-region (point-min) (point-max))
  (hs-interactive-mode-prompt (hs-project)))

(defun hs-interactive-mode-raise (project msg)
  "Raise an error buffer."
  (lexical-let
      ((current (current-buffer))
       (error-buffer (get-buffer-create (format "*hs-error-%s*"
                                                (hs-project-name project))))
       (map (make-keymap)))
    (switch-to-buffer-other-window error-buffer)
    (let ((inhibit-read-only t))
      (set-text-properties (point-min) (point-max) nil))
    (delete-region (point-min) (point-max))
    (insert "Interactive compile error:\n\n")
    (insert (propertize (format "%s\n" msg)
                        'face 'hs-faces-ghci-error
                        'read-only t
                        'rear-nonsticky t
                        'error t))
    (insert "\n(Hit q to close this buffer and return to the prompt.)\n")
    (let* ((cols (string-match "<interactive>:[0-9]+:\\([0-9]+\\)\-?\\([0-9]*\\)"
                               msg))
           (col-start (when cols (string-to-number (match-string 1 msg))))
           (col-end (when cols (max (1+ (string-to-number (match-string 2 msg)))
                                    col-start))))
      (when cols
        (with-current-buffer current
          (let ((point-start (save-excursion
                               (goto-char (point-max))
                               (goto-char (line-beginning-position))
                               (search-forward-regexp hs-config-buffer-prompt))))
            (add-text-properties (+ point-start col-start)
                                 (+ point-start col-end)
                                 '(face hs-faces-ghci-error))))))
    (define-key map (kbd "q")
      (lambda ()
        (interactive)
        (kill-buffer)
        (switch-to-buffer-other-window current)
        (with-current-buffer current
          (save-excursion
            (let ((point-start (progn (goto-char (point-max))
                                      (goto-char (line-beginning-position))
                                      (search-forward-regexp hs-config-buffer-prompt))))
              (remove-text-properties point-start
                                      (line-end-position)
                                      '(face hs-faces-ghci-error)))))))
    (use-local-map map)))

(provide 'hs-interactive-mode)
