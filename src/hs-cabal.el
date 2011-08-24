;;; hs-process.el — Interaction with the inferior process.

;; Copyright (C) 2011 Chris Done
;; Copyright (C) 2007, 2008 Stefan Monnier

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

(defun hs-cabal ())

(defun hs-cabal-build-interactive ()
  "Build the current Cabal project."
  (interactive)
  (hs-cabal-build (hs-project)))

(defun hs-cabal-ido-interactive ()
  "Interactively choose a cabal command to run."
  (interactive)
  (hs-cabal-arbitrary-command
   (hs-project)
   (read-from-minibuffer 
    (hs-lang-cabal-ido-command)
    (ido-completing-read (hs-lang-cabal-ido-command) hs-cabal-commands))))

(defun hs-cabal-script-interactive (&optional cmd)
  "Run a Cabal command."
  (interactive)
  (hs-process-arbitrary-command
   (hs-project)
   (concat
    ":!cd "
    (hs-project-cabal-dir (hs-project))
    " && "
    (or cmd
        (read-from-minibuffer
         (hs-lang-cabal-ido-script)
         (ido-completing-read (hs-lang-cabal-ido-script) hs-config-scripts))))))

(defun hs-cabal-command (project command)
  "Send the Cabal build command."
  (setf (hs-process-cmd (hs-project-process project)) 'arbitrary)
  (process-send-string
   (hs-process-process (hs-project-process project))
   (concat ":!cd " (hs-cabal-dir project)
           " && " (if hs-config-use-cabal-dev
                      hs-config-cabal-dev-bin
                    "cabal") 
           " " command
           " && cd " (hs-process-current-dir (hs-project-process project)) "\n")))

(defvar hs-cabal-commands
  '("install"
    "update"
    "list"
    "info"
    "upgrade"
    "fetch"
    "unpack"
    "check"
    "sdist"
    "upload"
    "report"
    "init"
    "configure"
    "build"
    "copy"
    "haddock"
    "clean"
    "hscolour"
    "register"
    "test"
    "help"))

(defun hs-cabal-build (project)
  "Cabal build the given project."
  (hs-cabal-command project "build")
  (setf (hs-process-cmd (hs-project-process project)) 'build))

(defun hs-cabal-arbitrary-command (project command)
  "Run an arbitrary Cabal command."
  (hs-interactive-mode-echo-read-only
   project
   (concat (hs-lang-arbitrary-cabal-output)
           "\n"))
  (hs-cabal-command project command))

(defun hs-cabal-dir (project)
  "Get the Cabal project dir."
  (if (hs-project-cabal-dir project)
      (hs-project-cabal-dir project)
    (setf (hs-project-cabal-dir project)
          (read-from-minibuffer 
           (hs-lang-cabal-project-path)
           (hs-process-current-dir
            (hs-project-process project))))))

(defun hs-cabal-find-file ()
  "Return a buffer visiting the cabal file of the current directory, or nil."
  (catch 'found
    (let ((user (nth 2 (file-attributes default-directory)))
          ;; Abbreviate, so as to stop when we cross ~/.
          (root (abbreviate-file-name default-directory))
          files)
      (while (and root (equal user (nth 2 (file-attributes root))))
        (if (setq files (directory-files root 'full "\\.cabal\\'"))
            ;; Avoid the .cabal directory.
            (dolist (file files (throw 'found nil))
              (unless (file-directory-p file)
                (throw 'found file)))
          (if (equal root
                     (setq root (file-name-directory
                                 (directory-file-name root))))
              (setq root nil))))
      nil)))

(defun hs-cabal-find-dir ()
  "Use the .cabal file-finding function to find a dir."
  (let ((file (hs-cabal-find-file)))
    (when file
      (file-name-directory file))))

(defun hs-cabal-get-dir ()
  "Get the Cabal dir for a new project. Various ways of figuring this out,
   and indeed just prompting the user. Do them all."
  (let* ((file (hs-cabal-find-file))
         (dir (when file (file-name-directory file))))
    (read-from-minibuffer
     (apply 'hs-lang-cabal-dir (when dir (list file)))
     (or dir default-directory))))

(defun hs-cabal-create ()
  "Create a cabal file."
  (interactive)
  (let* ((package-name (read-from-minibuffer "Package name: "))
         (version (read-from-minibuffer "Version: " "0.1"))
         (license (ido-completing-read
                   "License: "
                   '("BSD3"
                     "GPL"
                     "GPL-2"
                     "GPL-3"
                     "LGPL"
                     "LGPL-2.1"
                     "LGPL-3"
                     "MIT"
                     "PublicDomain"
                     "AllRightsReserved"
                     "OtherLicense")))
         (author-name (read-from-minibuffer "Author name: "))
         (maintainer-email (read-from-minibuffer "Maintainer email: "))
         (project-homepage (read-from-minibuffer
                            "Project homepage/repo: "))
         (synopsis (read-from-minibuffer "Synopsis: "))
         (category (ido-completing-read "Category: "
                                        hs-config-cabal-categories))
         (package-type (ido-completing-read "Package type: "
                                            '("Library"
                                              "Executable")))
         (setup-contents "import Distribution.Simple\nmain = defaultMain")
         (cabal-contents
          (concat "-- You should fill in a longer description"
                  " when you publish to Hackage.\n"
                  "-- \n"
                  "-- Hit `C-c c' to configure with this Cabal file.\n"
                  "-- Hit `C-c C-c' to build this Cabal project in"
                  "this or one of its associated files.\n"
                  "\n"
                  "Name: " package-name "\n"
                  "Version: " version "\n"
                  "Synopsis: " synopsis "\n"
                  "Description: " synopsis "\n"
                  "Homepage: " project-homepage "\n"
                  "-- You should put a LICENSE file in "
                  "your directory when you publish to Hackage.\n"
                  "License: " license "\n"
                  "Author: " author-name "\n"
                  "Maintainer: " maintainer-email "\n"
                  "Copyright: " (format-time-string "%Y") " by " author-name "\n"
                  "Category: " category "\n"
                  "Build-type: " "Simple" "\n"
                  "Cabal-version: " ">=1.2\n\n"
                  (if (string= "Library" package-type)
                      (concat "Library\n"
                              "-- Exposed-modules: \n"
                              "-- Build-depends: \n"
                              "-- Other-modules: \n"
                              "-- Build-tools: \n")
                    (concat "Executable " package-name "\n"
                            "  -- .hs containing the Main module.\n"
                            "  Main-is:             Main.hs\n"
                            "  Hs-source-dirs:      src\n"
                            "  -- Packages needed in order to build this package.\n"
                            "  Build-depends:       base > 4 && < 5\n"
                            "\n"
                            "  -- Modules not exported by this package "
                            "but required when distributing for Hackage.\n"
                            "  -- Other-modules:       \n"
                            "\n"
                            "  -- Extra tools (e.g. alex, hsc2hs, ...):"
                            "  -- Build-tools:  "))))
         (main-contents (format (concat "{-# OPTIONS -Wall #-}\n\n"
                                        "-- | Main entry point to project %s.\n\n"
                                        "module Main where\n\n"
                                        "-- | Main entry point.\n"
                                        "main :: IO ()\n"
                                        "main = return ()\n")
                                package-name))
         (cabal-file (concat package-name ".cabal"))
         (cabal-buffer (get-buffer-create cabal-file))
         (setup-buffer (get-buffer-create "Setup.hs"))
         (main-buffer (when (string= "Executable" package-type)
                        (get-buffer-create "Main.hs")))
         (directory (hs-cabal-get-directory
                     (format "A .cabal%s and Setup.hs buffer have been created. "
                             (if (string= "Executable" package-type)
                                 ", Main.hs"
                               "")))))
    (when (string= "Executable" package-type)
      (make-directory (concat directory "/src") t))
    (with-current-buffer cabal-buffer
      (insert cabal-contents)
      (write-file (concat directory "/" cabal-file)))
    (with-current-buffer setup-buffer
      (insert setup-contents)
      (write-file (concat directory "/" "Setup.hs")))
    (when (string= "Executable" package-type)
      (with-current-buffer main-buffer
        (insert main-contents)
        (write-file (concat directory "/src/Main.hs"))))
    (switch-to-buffer cabal-buffer)))

(defun hs-cabal-get-directory (msg)
  "Prompt for a directory, keep asking until we get one that
exists or to create one."
  (let ((directory (read-from-minibuffer
                    (concat msg
                            "Project directory to save them in: ")
                    default-directory))) 
    (if (file-directory-p directory)
        directory
      (if (y-or-n-p "Directory doesn't exist, create it [and parents]? ")
          (progn (make-directory directory t)
                 directory)
        (hs-cabal-get-directory "")))))

(provide 'hs-cabal)