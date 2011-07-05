;;; hs-lang-en.el — Provides English language strings.

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

(defun hs-lang-en ())

(defun hs-lang-process-ended (event)
  (format "The inferior Haskell process ended: %s"
          event))

(defun hs-lang-new-project-name ()
  "New project name: ")

(defun hs-lang-choose-project ()
  "Choose project: ")

(defun hs-lang-create-new-project (name)
  (format "Create new project “%s”? " name))

(defun hs-lang-welcome-message ()
  "Welcome to Haskell. Prepare your face.")

(defun hs-lang-cd-directory ()
  "Directory: ")

(defun hs-lang-tags-table-updated ()
  "Tags table updated.")

(defun hs-lang-arbitrary-command-finished ()
  "Command finished.")

(defun hs-lang-build-done ()
  "Done.")

(defun hs-lang-build-processing-executables (project)
  (format "Processing: %s" project))

(defun hs-lang-build-building (project)
  (format "Building: %s" project))

(defun hs-lang-build-linking (project)
  (format "Linking: %s" project))

(defun hs-lang-build-compilation-failed ()
  "Compilation failed.")

(defun hs-lang-load-ok ()
  "OK.")

(defun hs-lang-packages-flags-changed-resetting ()
  "Package flags changed, resetting and reloading.")

(defun hs-lang-compiling (module &optional file)
  (format "Compiling: %s%s"
          module
          (if file
              (format " [%s]" file)
            "")))

(defun hs-lang-directory-change (dir)
  (format "Directory change: %s" dir))

(defun hs-lang-directory-change-reload ()
  "The directory was changed. Please run reload again.")

(defun hs-lang-directory-does-not-exist (dir)
  (format "Directory %s does not exist." dir))

(defun hs-lang-command-output ()
  "Command output:")

(defun hs-lang-cabal-ido-command ()
  "Command: ")

(defun hs-lang-cabal-ido-script ()
  "Script: ")

(defun hs-lang-arbitrary-cabal-output ()
  "Cabal output:")

(defun hs-lang-cabal-project-path ()
  "Cabal project path: ")

(defun hs-lang-cabal-dir (&optional cabal-file)
  (format "Cabal dir%s: "
          (if cabal-file
              (format " (%s)" (file-name-nondirectory cabal-file))
            " (no cabal file)")))

(defun hs-lang-errors-unused () "Unused")
(defun hs-lang-errors-missing-signature () "Signature")
(defun hs-lang-errors-defaulting () "Defaulting")
(defun hs-lang-errors-mismatch () "Mismatch")
(defun hs-lang-errors-ambiguous () "Ambiguous")
(defun hs-lang-errors-illegal () "Illegal")
(defun hs-lang-errors-no-instance () "No instance")
(defun hs-lang-errors-could-not-deduce () "Deduce")
(defun hs-lang-errors-warning () "Warning")
(defun hs-lang-errors-error () "Error")
(defun hs-lang-errors-incomplete-do () "Incomplete `do'.")
(defun hs-lang-errors-x-against-y (x y)
  (format "“%s” ≠ “%s”" x y))
(defun hs-lang-errors-x-in-y (x y)
  (format "“%s” in “%s”" x y))

(provide 'hs-lang-en)
