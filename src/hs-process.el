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

(defun hs-process-type-of-interactive ()
  "Get the type of something interactively."
  (interactive)
  (hs-process-type-of (hs-project) (hs-ident-at-point)))

(defun hs-process-info-of-interactive ()
  "Get the info of something interactively."
  (interactive)
  (hs-process-info-of (hs-project) (hs-ident-at-point)))

(defun hs-process-info-of-passive-interactive (symbol)
  "Send an arbitrary command (no printing in the REPL)."
  (let ((project (hs-project)))
    (setf (hs-process-cmd (hs-project-process project)) 'info-of-passive)
    (process-send-string (hs-process-process (hs-project-process project))
                         (concat ":i " symbol "\n"))))

(defun hs-process-start (project &optional name)
  "Start the inferior haskell process."
  (let ((process (hs-process-make :cmd 'startup
                                  :name (if name
                                            (format "%s-%s"
                                                    (hs-project-name project)
                                                    name)
                                          (hs-project-name project))
                                  :response ""
                                  :response-cursor 0
                                  :load-dirs '()
                                  :current-dir nil
                                  :response-callback nil)))
    (hs-process-start-ghci project process)
    (if name
        (progn
          (set-process-sentinel (hs-process-process process)
                                'hs-process-slave-sentinel)
          (set-process-filter (hs-process-process process)
                              'hs-process-slave-filter))
      (progn (set-process-sentinel (hs-process-process process) 'hs-process-sentinel)
             (set-process-filter (hs-process-process process) 'hs-process-filter)))
    (process-send-string (hs-process-process process) (concat ":set prompt \"> \"\n"))
    (process-send-string (hs-process-process process) ":set -v1\n")
    (process-send-string (hs-process-process process) (concat "()\n"))
    process))

(defun hs-process-start-ghci (project process)
  (if hs-config-use-cabal-dev
      (setf (hs-process-process process)
            (let ((default-directory (hs-project-cabal-dir project))) 
              (start-process
               (hs-process-name process)
               nil
               hs-config-cabal-dev-bin
               "ghci"
               "-s"
               (hs-project-cabal-dev-dir project))))
    (setf (hs-process-process process)
          (start-process
           (hs-process-name process)
           nil
           hs-config-ghci-bin))))

(defun hs-process-filter (proc response)
  "The filter for the process pipe."
  (when hs-config-echo-all (message response))
  (let ((project (hs-process-project-by-proc proc)))
    (when project
      (when (not (eq (hs-process-cmd (hs-project-process project))
                     'none))
        (hs-process-collect project
                            response
                            (hs-project-process project)
                            'main)))))

(defun hs-process-slave-filter (proc response)
  "The filter for the slave process pipe."
  (when hs-config-echo-all (message response))
  (let ((project (hs-process-project-by-proc proc "slave")))
    (when project
      (when (not (eq (hs-process-cmd (hs-project-slave-process project))
                     'none))
        (hs-process-collect project
                            response
                            (hs-project-slave-process project)
                            'slave)))))

(defun hs-process-sentinel (proc event)
  "The sentinel for the process pipe."
  (let ((event (replace-regexp-in-string "\n$" "" event)))
    (message (hs-lang-process-ended event))
    (let ((project (hs-process-project-by-proc proc)))
      (when project
        (hs-interactive-mode-echo-error project (hs-lang-process-ended event))
        (when (and (eq 'eval (hs-process-cmd (hs-project-process project)))
                   (string= (hs-interactive-mode-input project)
                            ":q"))
          (hs-interactive-mode-set-prompt ""))
        (hs-process-prompt-restart project)))))

(defun hs-process-slave-sentinel (proc event)
  "The sentinel for the process pipe."
  (let ((event (replace-regexp-in-string "\n$" "" event)))
    (message (hs-lang-process-ended event))
    (let ((project (hs-process-project-by-proc proc "slave")))
      (when project
        (hs-interactive-mode-echo-error project (hs-lang-process-ended event))
        (when (and (eq 'eval (hs-process-cmd (hs-project-slave-process project)))
                   (string= (hs-interactive-mode-input project)
                            ":q"))
          (hs-interactive-mode-set-prompt ""))
        (hs-process-slave-prompt-restart project)))))

(defun hs-process-prompt-restart (project)
  (when (y-or-n-p "The Haskell process died. Restart? ")
    (setf (hs-project-process project)
          (hs-process-start project))))

(defun hs-process-slave-prompt-restart (project)
  (when (y-or-n-p "The slave Haskell process died. Restart? ")
    (setf (hs-project-slave-process project)
          (hs-process-start project "slave"))))

(defun hs-process-project-by-proc (proc &optional slave)
  "Find project by process."
  (find-if (lambda (project)
             (if slave
                 (string= (concat (hs-project-name project) "-" slave)
                          (process-name proc))
               (string= (hs-project-name project)
                        (process-name proc))))
           *hs-projects*))

(defun hs-process-collect (project response process type)
  "Collect input for the response until receives a prompt."
;  (message (format "%s: %s" (hs-process-name process) response))
  (setf (hs-process-response process)
        (concat (hs-process-response process) response))
  (while (hs-process-live-updates project process))
  (when (string-match hs-config-process-prompt-regex
                      (hs-process-response process))
    (when (hs-process-response-handler
           project
           process
           (replace-regexp-in-string
            hs-config-process-prompt-regex
            ""
            (hs-process-response process))
           type)
      (hs-process-reset process))))

(defun hs-process-reset (process)
  (progn (setf (hs-process-response-cursor process) 0)
         (setf (hs-process-response process) "")
         (setf (hs-process-cmd process) 'none)
         (setf (hs-process-response-callback process) nil)))

(defun hs-process-response-handler (project process response type)
  "Handle receiving a response."
  (ecase (hs-process-cmd process)
    ('startup (hs-process-reset process)
              (with-current-buffer (hs-interactive-mode-name project)
                (hs-completion))
              (hs-interactive-mode-echo-read-only
               project (hs-lang-welcome-message)))
    ('eval (progn (when (not (string= "" response))
                    (hs-interactive-mode-eval-insert-result project response))
                  (hs-interactive-mode-prompt project)
                  t))
    ('load-file t)
    ('tags-generate (progn (let ((tags-revert-without-query t))
                             (when (hs-process-current-dir process)
                               (visit-tags-table (hs-process-current-dir process))
                               (hs-message-line (hs-lang-tags-table-updated))))
                           t))
    ('arbitrary (progn (hs-interactive-mode-echo-read-only 
                        project
                        (hs-lang-arbitrary-command-finished))
                       (hs-message-line (hs-lang-arbitrary-command-finished))
                       t))
    ('background-arbitrary (progn (hs-message-line (hs-lang-arbitrary-command-finished))
                                  t))
    ('type-of (progn (hs-message-line response)
                     (hs-interactive-mode-echo-type project response)
                     t))
    ('info-of (progn (hs-message-line response)
                     (hs-interactive-mode-echo-type project response)
                     t))
    ('info-of-passive (progn (hs-message-line response)
                             t))
    ('build
     (if (hs-process-consume
          process
          "^cabal: .+?.cabal has been changed, please re-configure.")
         (progn (hs-interactive-mode-echo-error project (hs-lang-config-changed))
                (hs-message-line (hs-lang-config-changed)))
       (progn
         (let ((cursor (hs-process-response-cursor process)))
           (setf (hs-process-response-cursor process) 0)
           (let ((error-counter 0))
             (while (hs-process-trigger-type-errors-warnings project process)))
           (setf (hs-process-response-cursor process) cursor))
         (hs-interactive-mode-echo-read-only project (hs-lang-build-done))
         (hs-message-line (hs-lang-build-done))))
     t)
    ('none)))

(defun hs-process-live-updates (project process)
  "Trigger any updates that happen during receiving a response."
  (case (hs-process-cmd process)
    ('arbitrary (hs-process-trigger-arbitrary-updates project process))
    ('load-file (hs-process-trigger-build-updates project process))
    ('startup (hs-process-trigger-build-updates project process))
    ('build (hs-process-trigger-build-updates project process))
    ('eval (hs-process-trigger-eval-errors project process))))

(defun hs-process-trigger-eval-errors (project process)
  "Trigger evaluation errors like compile messages or exceptions."
  (cond 
   ((hs-process-consume
     process
     "^<interactive>:\\([0-9]+\\):\\([0-9]+\\):[ \r\n]\\([[:unibyte:]]+?\\)\n>")
    (let ((error-msg
           (replace-regexp-in-string
            "^    "
            ""
            (match-string 3 (hs-process-response process)))))
      (if (string-match "^[ ]*Warning:" error-msg)
          (let ((cursor (hs-process-response-cursor process)))
            (setf (hs-process-response-cursor process) 0)
            (let ((error-counter 0)) 
              (while (hs-process-trigger-type-errors-warnings project process)))
            (setf (hs-process-response process)
                  (substring (hs-process-response process)
                             (hs-process-response-cursor process)))
            (setf (hs-process-response-cursor process) 0))
        (progn (hs-interactive-mode-raise (hs-project) error-msg)
               (hs-process-reset process)))))))

(defun hs-process-trigger-build-updates (project process)
  "Trigger the 'loading module' updates if any."
  (cond
   ((hs-process-consume
     process
     (concat "\\[\\([0-9]+\\) of \\([0-9]+\\)\\]"
             " Compiling \\([^ ]+\\)[ ]+"
             "( \\([^ ]+\\), \\([^ ]+\\) )[\r\n]+"))
    (hs-process-show-load-message
     project
     process
     (match-string 3 (hs-process-response process))
     (match-string 4 (hs-process-response process)))
    t)
   ((hs-process-consume process "^Preprocessing executables for \\(.+?\\)\\.\\.\\.")
    (hs-interactive-mode-echo-read-only
     project
     (hs-lang-build-processing-executables
      (match-string 1 (hs-process-response process)))))
   ((hs-process-consume process "\nBuilding \\(.+?\\)\\.\\.\\.")
    (hs-interactive-mode-echo-read-only
     project
     (hs-lang-build-building
      (match-string 1 (hs-process-response process)))))
   ((hs-process-consume process "Linking \\(.+?\\) \\.\\.\\.")
    (let ((msg (hs-lang-build-linking
                (match-string 1 (hs-process-response process)))))
      (hs-interactive-mode-echo-read-only project msg)
      (hs-message-line msg)))
   ((hs-process-consume process "Failed, modules loaded: \\(.+\\)$")
    (let ((cursor (hs-process-response-cursor process)))
      (setf (hs-process-response-cursor process) 0)
      (let ((error-counter 0))
        (while (hs-process-trigger-type-errors-warnings project process)
          (setq error-counter (1+ error-counter))))
      (setf (hs-process-response-cursor process) cursor)
      (hs-interactive-mode-echo-error project (hs-lang-build-compilation-failed-simple)))
    t)
   ((hs-process-consume process "Ok, modules loaded: \\(.+\\)$")
    (let ((cursor (hs-process-response-cursor process)))
      (setf (hs-process-response-cursor process) 0)
      (let ((error-counter 0) (warning-count 0))
        (while (hs-process-trigger-type-errors-warnings project process)
          (setq warning-count (1+ warning-count)))
        (setf (hs-process-response-cursor process) cursor)
        (hs-message-line (hs-lang-load-ok warning-count))))
    t)
   ((hs-process-consume process "Loading package \\([^ ]+\\) ... linking ... done.\n")
    (hs-message-line
     (format "Loading: %s"
             (match-string 1 (hs-process-response process)))))
   ((hs-process-consume process
                        "package flags have changed, resetting and loading new packages...")
    (hs-message-line (hs-lang-packages-flags-changed-resetting)))))

(defun hs-process-trigger-type-errors-warnings (project process)
  "Trigger handling type errors or warnings."
  (cond
   ((hs-process-consume
     process
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
           (echo (if warning
                     #'hs-interactive-mode-echo-warning
                   #'hs-interactive-mode-echo-error))
           (contextual (lambda (x)
                         (format "%s: %s"
                                 (hs-errors-message-type error-msg warning)
                                 x)))
           (final-msg (format "%s:%s:%s: %s" 
                              (hs-process-strip-dir project file)
                              (hs-errors-line error-msg line)
                              col
                              (hs-errors-reduce-error-msg preview-msg))))
      (funcall echo project (funcall contextual final-msg))
      (hs-process-trigger-extension-suggestions project error-msg)
      (when (and (= error-counter 0) (not warning))
        (hs-message-line (hs-lang-build-compilation-failed final-msg))))
    t)))

(defun hs-process-trigger-extension-suggestions (project msg)
  "Trigger prompting to add any extension suggestions."
  (when (string-match "\\-X\\([A-Z][A-Za-z]+\\)" msg)
    (let* ((extension (match-string 1 msg))
           (string (format "{-# LANGUAGE %s #-}" extension)))
      (when (y-or-n-p (format "Add %s to the top of the file? "
                              string))
        (hs-mode-insert-at-top string)))))

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

(defun hs-process-consume (process regex)
  "Consume a regex from the response and move the cursor along if succeed."
  (when (string-match regex
                      (hs-process-response process)
                      (hs-process-response-cursor process))
    (setf (hs-process-response-cursor process)
          (match-end 0))
    t))

(defun hs-process-show-load-message (project process module-name file-name)
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
    (hs-message-line msg)
    (when (eq (hs-process-cmd process) 'build)
      (hs-interactive-mode-echo-read-only project msg))))

(defun hs-process-trigger-arbitrary-updates (project processs)
  "Just log out any arbitrary output."
  (let* ((new-data (substring (hs-process-response process)
                              (hs-process-response-cursor process))))
    (hs-interactive-mode-echo-read-only-incomplete
     project
     (replace-regexp-in-string "\n> $" "" new-data)) ;; Not a reliable method, but it'll do.
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
      (progn (hs-process-cd-interactive)
             (hs-interactive-mode-echo-read-only
              project
              (hs-lang-directory-change-reload)))
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
        (setf (hs-process-current-dir (hs-project-slave-process project)) dir)
        (process-send-string (hs-process-process (hs-project-process project))
                             (concat ":cd " dir "\n"))
        (process-send-string (hs-process-process (hs-project-slave-process project))
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

(defun hs-process-type-of (project symbol)
  "Send an arbitrary command (no printing in the REPL)."
  (setf (hs-process-cmd (hs-project-process project)) 'type-of)
  (process-send-string (hs-process-process (hs-project-process project))
                       (concat ":t " symbol "\n")))

(defun hs-process-info-of (project symbol)
  "Send an arbitrary command (no printing in the REPL)."
  (setf (hs-process-cmd (hs-project-process project)) 'info-of)
  (process-send-string (hs-process-process (hs-project-process project))
                       (concat ":i " symbol "\n")))

(provide 'hs-process)
