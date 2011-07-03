;;; hs-lang-pirate.el — Provides Pirate English language strings.

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

(defun hs-lang-pirate ())

(defun hs-lang-new-project-name ()
  "Aye, what be the name? ")

(defun hs-lang-choose-project ()
  "Any last words? Pick yer final meal and let it be worsten shite! ")

(defun hs-lang-create-new-project (name)
  (format "Arrgh! No treasure here! Burry “%s”? " name))

(defun hs-lang-welcome-message ()
  "YARR! welcome yerselfs on me fine vessel! Haskell be 'er name, the seas be 'er ways.")

(defun hs-lang-cd-directory ()
  "Where we headin'?: ")

(defun hs-lang-tags-table-updated ()
  "AYE! Treasure chest filled with doubloons and all sortsa riches!")

(defun hs-lang-arbitrary-command-finished ()
  "I've finished all me crog! Spare a swig for yer best shipmate?")

(defun hs-lang-build-done ()
  "We've done it, Cap'n!")

(defun hs-lang-build-processing-executables (project)
  (format "Lookin' at %s, smartly." project))

(defun hs-lang-build-building (project)
  (format "Workin' the sails and the %s, by the sweat of our backs!" project))

(defun hs-lang-build-linking (project)
  (format "Burrying %s where no scoundrel will ever find it, yar!" project))

(defun hs-lang-build-compilation-failed ()
  "ABANDON SHIP! DON'T FORGET THE RUM!")

(defun hs-lang-load-ok ()
  "YAR!")

(defun hs-lang-packages-flags-changed-resetting ()
  "Aye, the wind be changin' direction...")

(defun hs-lang-compiling (module &optional file)
  (format "Craftin' a %s%s"
          module
          (if file
              (format ", but 'er REAL name be %s" file)
            "")))

(defun hs-lang-directory-change (dir)
  (format "New 'eadin', sir! To %s, an' beyond! ARRR!" dir))

(defun hs-lang-directory-change-reload ()
  "The winds be changin'. Run reload again and be quick about it!")

(defun hs-lang-directory-does-not-exist (dir)
  (format "There be no cargo named %s! Be that a kind of treasure?" dir))

(defun hs-lang-command-output ()
  "Quoth the cowardly death threat:")

(defun hs-lang-cabal-ido-command ()
  "What shall ye 'ave me do, Cap'n? ")

(defun hs-lang-cabal-ido-script ()
  "What script contains the locations on me treasures? ")

(defun hs-lang-arbitrary-cabal-output ()
  "The Kraken let out a great roar!")

(defun hs-lang-cabal-project-path ()
  "On what bearing the Kraken be? ")

(defun hs-lang-cabal-dir (&optional cabal-file)
  (format "Bearin' of the Kraken%s: "
          (if cabal-file
              (format " (guessin' %s be related)" (file-name-nondirectory cabal-file))
            "")))

(defun hs-lang-errors-unused () "Weren't even nobody usin' it anyway!")
(defun hs-lang-errors-missing-signature () "Where's the bloomin' sign?")
(defun hs-lang-errors-defaulting () "We tossed a coin, and defaulted, yar!")
(defun hs-lang-errors-mismatch () "These treasure directions don't match!")
(defun hs-lang-errors-ambiguous () "Ain't sure")
(defun hs-lang-errors-illegal () "Conduct thee'self rightwise or ye'll get shot!")
(defun hs-lang-errors-no-instance () "There ain't no such creature!")
(defun hs-lang-errors-could-not-deduce () "Couldn't find the treasure with that map")
(defun hs-lang-errors-warning () "That be a warning shot, arr!")
(defun hs-lang-errors-error () "Ye make a grave m'stake")
(defun hs-lang-errors-incomplete-do () "Do what, Cap'n?")
(defun hs-lang-errors-x-against-y (x y)
  (format "“%s” don't match “%s” any side of the sea!" x y))
(defun hs-lang-errors-x-in-y (x y)
  (format "“%s” in “%s” an that." x y))

(provide 'hs-lang-pirate)
