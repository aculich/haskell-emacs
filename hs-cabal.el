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
    "Command: "
    (ido-completing-read "Command: " hs-cabal-commands))))

(defun hs-cabal-script-interactive ()
  (interactive)
  (hs-process-arbitrary-command
   (hs-project)
   (concat
    ":!cd "
    (hs-project-cabal-dir (hs-project))
    " && "
    (read-from-minibuffer
     "Command: "
     (ido-completing-read "Command: " hs-config-scripts)))))

(defun hs-cabal-command (project command)
  "Send the Cabal build command."
  (setf (hs-process-cmd (hs-project-process project)) 'arbitrary)
  (process-send-string
   (hs-process-process (hs-project-process project))
   (concat ":!cd " (hs-cabal-dir project)
           " && cabal-dev " command
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
  (hs-buffer-echo-read-only project "Cabal output:\n")
  (hs-cabal-command project command))

(defun hs-cabal-dir (project)
  "Get the Cabal project dir."
  (if (hs-project-cabal-dir project)
      (hs-project-cabal-dir project)
    (setf (hs-project-cabal-dir project)
          (read-from-minibuffer 
           "Cabal project path: "
           (hs-process-current-dir
            (hs-project-process project))))))

(provide 'hs-cabal)