;;; hs-project.el — Project-specific actions.

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

(defun hs-project-start (&optional name)
  (interactive)
  (hs)
  (let ((project (hs-project-create name)))
    (switch-to-buffer (hs-interactive-mode-buffer project))))

(defun hs-project (&optional dont-prompt)
  "Get the current project. Default if there's only one, prompt
   to choose if there're more than one, prompt to create if
   there're none."
  (if *hs-project*
      *hs-project*
    (or (hs-project-figure-out-from-buffer-path)
        (hs-project-guess-from-buffer-path)
        (hs-project-switch-or-create dont-prompt))))

(defun hs-project-guess-from-buffer-path ()
  "Try a guess of a new project based on the buffer filename path."
  (when (y-or-n-p (format "Start a new project named “%s”? "
                          (hs-project-default-project-name)))
    (hs-project-create (hs-project-default-project-name))))

(defun hs-project-figure-out-from-buffer-path ()
  "Figure out the current buffer's project based on the filename path."
  (let ((project
         (remove-if-not
          (lambda (project) 
            (and (buffer-file-name)
                 (hs-is-prefix-of (hs-project-cabal-dir project) (buffer-file-name)))) 
          *hs-projects*)))
    (when (consp project) (hs-project-choose (car project)))))

(defun hs-project-switch-or-create (&optional dont-prompt)
  "Switch the project or create one."
  (interactive)
  (let ((project
         (unless dont-prompt
           (if (null *hs-projects*)
               (hs-project-create)
             (hs-project-choose)))))
    (when project
      (hs-project-choose project)
      project)))

(defun hs-project-choose (&optional project)
  "Set the project of the current buffer."
  (if project
      (progn (make-local-variable '*hs-project*)
             (setq *hs-project* project)
             project)
    (hs-project-choose
     (let ((name (ido-completing-read
                  (hs-lang-choose-project)
                  (mapcar 'hs-project-name *hs-projects*))))
       (when name
         (let ((project (find-if (lambda (project)
                                   (string= (hs-project-name project)
                                            name))
                                 *hs-projects*)))
           (if project
               project
             (when (y-or-n-p (hs-lang-create-new-project name))
               (hs-project-create name)))))))))

(defun hs-project-default-project-name ()
  (or (let ((file (hs-cabal-find-file)))
        (when file
          (downcase (file-name-sans-extension
                     (file-name-nondirectory file)))))
      hs-config-default-project-name))

(defun hs-project-create (&optional provided-name)
  "Create a new project, prompt for a name if requested."
  (interactive)
  (let ((name (or provided-name
                  (read-from-minibuffer (hs-lang-new-project-name)
                                        (hs-project-default-project-name))))
        (cabal-dir (hs-cabal-get-dir)))
    (let* ((project (hs-project-make 
                     :process nil
                     :slave-process nil
                     :name name
                     :cabal-dir cabal-dir
                     :cabal-dev-dir (concat cabal-dir "/cabal-dev")
                     :prompt-history '()))
           (buffer (hs-interactive-mode-create project)))
      (setf (hs-project-process project)
            (hs-process-start project))
      (setf (hs-project-slave-process project)
            (hs-process-start project "slave"))
      (add-to-list '*hs-projects* project)
      project)))

(provide 'hs-project)
