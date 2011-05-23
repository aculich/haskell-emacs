;;; hs-process.el — Interaction with the inferior process.

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

(defun hs-process ())

(defun hs-process-cd-interactive ()
  "Change directory."
  (interactive)
  (let ((dir (read-from-minibuffer
              (hs-lang-cd-directory)
              (or (hs-process-current-dir (hs-project-process (hs-project)))
                  (if (buffer-file-name)
                      (file-name-directory (buffer-file-name))
                    "~/")))))
    (hs-process-cd (hs-project) dir)))

(defun hs-process-load-interactive ()
  "Load the file in the current buffer."
  (interactive)
  (hs-process-load-file (hs-project) (buffer-file-name)))

(defun hs-process-start (project)
  "Start the inferior haskell process."
  (let ((process (hs-process-make :cmd 'startup
                                  :name (hs-project-name project)
                                  :response ""
                                  :response-cursor 0
                                  :load-dirs '()
                                  :current-dir nil
                                  :response-callback nil)))
    (setf (hs-process-process process)
          (start-process
           (hs-process-name process)
           nil
           hs-config-cabal-dev-bin
           "ghci"
           "-s"
           (hs-project-cabal-dev-dir project)))
    (set-process-filter (hs-process-process process) 'hs-process-filter)
    (process-send-string (hs-process-process process) (concat ":set prompt \"> \"\n"))
    (process-send-string (hs-process-process process) ":set -v1\n")
    (setf (hs-project-process project) process)
    process))

(defun hs-process-filter (proc response)
  "The filter for the process pipe."
  (let ((project (hs-process-project-by-proc proc)))
    (when project
      (when (not (eq (hs-process-cmd (hs-project-process project))
                     'none))
        (hs-process-collect project response)))))

(defun hs-process-project-by-proc (proc)
  "Find project by process."
  (find-if (lambda (project)
             (string= (hs-project-name project)
                      (process-name proc)))
           *hs-projects*))

(defun hs-process-collect (project response)
  "Collect input for the response until receives a prompt."
  (let ((process (hs-project-process project)))
    (setf (hs-process-response process)
          (concat (hs-process-response process) response))
    (while (hs-process-live-updates project))
    (when (string-match hs-config-process-prompt-regex
                        (hs-process-response process))
      (when (hs-process-response-handler
             project
             (replace-regexp-in-string
              hs-config-process-prompt-regex
              ""
              (hs-process-response process)))
        (progn (setf (hs-process-response-cursor process) 0)
               (setf (hs-process-response process) "")
               (setf (hs-process-cmd process) 'none)
               (setf (hs-process-response-callback process) nil))))))

(defun hs-process-response-handler (project response)
  "Handle receiving a type response."
  (let ((process (hs-project-process project)))
    (ecase (hs-process-cmd process)
      ('startup t)
      ('eval (progn (when (not (string= "" response))
                      (hs-interactive-mode-eval-insert-result project response))
                    (hs-interactive-mode-prompt project)
                    t))
      ('load-file (progn t))
      ('tags-generate (progn (let ((tags-revert-without-query t))
                               (visit-tags-table (hs-process-current-dir process))
                               (message (hs-lang-tags-table-updated)))
                             t))
      ('arbitrary (progn (hs-interactive-mode-echo-read-only 
                          project
                          (hs-lang-arbitrary-command-finished))
                         (message (hs-lang-arbitrary-command-finished))
                         t))
      ('background-arbitrary (progn (message (hs-lang-arbitrary-command-finished))
                                    t))
      ('build
       (let ((cursor (hs-process-response-cursor process)))
         (setf (hs-process-response-cursor process) 0)
         (while (hs-process-trigger-type-errors-warnings project))
         (setf (hs-process-response-cursor process) cursor))
       (hs-interactive-mode-echo-read-only project (hs-lang-build-done))
       (message (hs-lang-build-done))
       t))))

(defun hs-process-live-updates (project)
  "Trigger any updates that happen during receiving a response."
  (let ((process (hs-project-process project)))
    (case (hs-process-cmd process)
      ('arbitrary (hs-process-trigger-arbitrary-updates project))
      ('load-file (hs-process-trigger-build-updates project))
      ('startup (hs-process-trigger-build-updates project))
      ('build (hs-process-trigger-build-updates project)))))

(defun hs-process-trigger-build-updates (project)
  "Trigger the 'loading module' updates if any."
  (let ((process (hs-project-process project)))
    (cond
     ((hs-process-consume
       project
       (concat "\\[\\([0-9]+\\) of \\([0-9]+\\)\\]"
               " Compiling \\([^ ]+\\)[ ]+"
               "( \\([^ ]+\\), \\([^ ]+\\) )[\r\n]+"))
      (hs-process-show-load-message
       project
       (match-string 3 (hs-process-response (hs-project-process project)))
       (match-string 4 (hs-process-response (hs-project-process project))))
      t)
     ((hs-process-consume project "^Preprocessing executables for \\(.+?\\)\\.\\.\\.")
      (hs-interactive-mode-echo-read-only
       project
       (hs-lang-build-processing-executables
        (match-string 1 (hs-process-response (hs-project-process project))))))
     ((hs-process-consume project "\nBuilding \\(.+?\\)\\.\\.\\.")
      (hs-interactive-mode-echo-read-only
       project
       (hs-lang-build-building
        (match-string 1 (hs-process-response (hs-project-process project))))))
     ((hs-process-consume project "Linking \\(.+?\\) \\.\\.\\.")
      (let ((msg (hs-lang-build-linking
                  (match-string 1 (hs-process-response 
                                   (hs-project-process project))))))
        (hs-interactive-mode-echo-read-only project msg)
        (message msg)))
     ((hs-process-consume project "Failed, modules loaded: \\(.+\\)$")
      (let ((cursor (hs-process-response-cursor process)))
        (setf (hs-process-response-cursor process) 0)
        (while (hs-process-trigger-type-errors-warnings project))
        (setf (hs-process-response-cursor process) cursor)
        (hs-interactive-mode-echo-error project (hs-lang-build-compilation-failed))
        (message (hs-lang-build-compilation-failed)))
      t)
     ((hs-process-consume project "Ok, modules loaded: \\(.+\\)$")
      (let ((cursor (hs-process-response-cursor process)))
        (setf (hs-process-response-cursor process) 0)
        (while (hs-process-trigger-type-errors-warnings project))
        (setf (hs-process-response-cursor process) cursor)
        (message (hs-lang-load-ok)))
      t)
     ((hs-process-consume project "Loading package \\([^ ]+\\) ... linking ... done.\n")
      (message
       (format "Loading: %s"
               (match-string 1 (hs-process-response (hs-project-process project))))))
     ((hs-process-consume
       project
       "package flags have changed, resetting and loading new packages...")
      (message (hs-lang-packages-flags-changed-resetting))))))

(defun hs-process-trigger-type-errors-warnings (project)
  "Trigger handling type errors or warnings."
  (let ((process (hs-project-process project)))
    (cond
     ((hs-process-consume
       project
       (concat "[\r\n]\\([^ \r\n:][^:\n\r]+\\):\\([0-9]+\\):\\([0-9]+\\):"
               "[ \n\r]+\\([[:unibyte:]]+?\\)\n[^ ]"))
      (setf (hs-process-response-cursor process)
            (- (hs-process-response-cursor process) 1))
      (let* ((error-msg (match-string 4 (hs-process-response process)))
             (file (match-string 1 (hs-process-response process)))
             (line (match-string 2 (hs-process-response process)))
             (col (match-string 3 (hs-process-response process)))
             (warning (string-match "^Warning: " error-msg))
             (preview-msg (hs-process-preview-error-msg error-msg warning))
             (echo (if warning #'hs-interactive-mode-echo-warning #'hs-interactive-mode-echo-error)))
        (funcall echo project
                 (format "%s: %s:%s:%s: %s"
                         (hs-errors-message-type error-msg warning)
                         (hs-process-strip-dir project file)
                         line
                         col
                         (hs-errors-reduce-error-msg preview-msg))))
      t))))

(defun hs-process-preview-error-msg (msg warning)
  "Show a concise preview of an error message."
  (let ((first-line (replace-regexp-in-string "\n.*$" "" error-msg)))
    (if warning
        (replace-regexp-in-string "Warning: " "" first-line)
      (hs-errors-reduce-error-msg msg))))

(defun hs-process-strip-dir (project file)
  "Strip the load dir from the file path."
  (let ((cur-dir (hs-process-current-dir (hs-project-process project))))
    (if (> (length file) (length cur-dir))
        (if (string= (substring file 0 (length cur-dir))
                     cur-dir)
            (replace-regexp-in-string 
             "^[/\\]" ""
             (substring file 
                        (length cur-dir)))
          file)
      file)))

(defun hs-process-consume (project regex)
  "Consume a regex from the response and move the cursor along if succeed."
  (when (string-match regex
                      (hs-process-response (hs-project-process project))
                      (hs-process-response-cursor (hs-project-process project)))
    (setf (hs-process-response-cursor (hs-project-process project))
          (match-end 0))
    t))

(defun hs-process-show-load-message (project module-name file-name)
  "Show the 'Loading X' message."
  (let* ((file-name-module
          (replace-regexp-in-string
           "\\.hs$" ""
           (replace-regexp-in-string "[\\/]" "." file-name)))
         (msg (apply
               'hs-lang-compiling
               (cons module-name
                     (if (string= file-name-module module-name)
                         nil
                       (when hs-config-show-filename-in-load-messages
                         (list file-name)))))))
    (message msg)
    (when (eq (hs-process-cmd (hs-project-process project))
              'build)
      (hs-interactive-mode-echo-read-only project msg))))

(defun hs-process-trigger-arbitrary-updates (project)
  "Just log out any arbitrary output."
  (let* ((process (hs-project-process project))
         (new-data (substring (hs-process-response process)
                              (hs-process-response-cursor process))))
    (hs-interactive-mode-echo-read-only-incomplete project new-data)
    (mapc 'message (split-string new-data "\n"))
    (setf (hs-process-response-cursor process)
          (+ (hs-process-response-cursor process)
             (length new-data)))
    nil))

(defun hs-process-trigger-background-arbitrary-updates (process)
  "Just log out any arbitrary output."
  (let ((new-data (substring (hs-process-response process)
                             (hs-process-response-cursor process))))
    (mapc 'message (split-string new-data "\n"))
    (setf (hs-process-response-cursor process)
          (+ (hs-process-response-cursor process) (length new-data)))
    nil))

(defun hs-process-eval (project expr)
  "Evaluate an expression."
  (setf (hs-process-cmd (hs-project-process project)) 'eval)
  (process-send-string (hs-process-process (hs-project-process project))
                       (concat expr "\n")))

(defun hs-process-load-file (project &optional file)
  "Load a file."
  (if (not (hs-process-current-dir (hs-project-process project)))
      (hs-process-cd-interactive)
    (let ((file-name (if file
                         file
                       (buffer-file-name))))
      (setf (hs-process-cmd (hs-project-process project)) 'load-file)
      (process-send-string (hs-process-process (hs-project-process project))
                           (concat ":load " file-name "\n")))))

(defun hs-process-cd (project dir)
  "Change current directory of the REPL."
  (if (file-directory-p dir)
      (progn
        (setf (hs-process-current-dir (hs-project-process project)) dir)
        (process-send-string (hs-process-process (hs-project-process project))
                             (concat ":cd " dir "\n"))
        (hs-interactive-mode-echo-read-only
         project
         (hs-lang-directory-change dir))
        (with-current-buffer (hs-interactive-mode-name project)
          (cd dir)))
    (error (hs-lang-directory-does-not-exist dir))))

(defun hs-process-arbitrary-command (project cmd)
  "Send an arbitrary command."
  (setf (hs-process-cmd (hs-project-process project)) 'arbitrary)
  (hs-interactive-mode-echo-read-only project (concat (hs-lang-command-output) "\n"))
  (process-send-string (hs-process-process (hs-project-process project))
                       (concat cmd "\n")))

(defun hs-process-background-arbitrary-command (project cmd)
  "Send an arbitrary command (no printing in the REPL)."
  (setf (hs-process-cmd (hs-project-process project)) 'background-arbitrary)
  (process-send-string (hs-process-process (hs-project-process project))
                       (concat cmd "\n")))

(provide 'hs-process)
