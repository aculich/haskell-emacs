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
  "You know what the call a Quarter Pounder with cheese in France? ")

(defun hs-lang-choose-project ()
  "Mind if I try one of yours? This is yours here, right? ")

(defun hs-lang-create-new-project (name)
  (format "“%s” ain't no country I've ever heard of. They speak English in %s?"
          name name))

(defun hs-lang-welcome-message ()
  (concat "The path of the righteous man is beset on all sides by the "
          "iniquities of the selfish and the tyranny of evil men. Blessed "
          "is he who, in the name of charity and good will, shepherds the "
          "weak through the valley of darkness, for he is truly his "
          "brother's keeper and the finder of lost children. And I will "
          "strike down upon thee with great vengeance and furious anger "
          "those who would attempt to poison and destroy My brothers. And "
          "you will know My name is the Lord when I lay My vengeance upon "
          "thee."))

(defun hs-lang-cd-directory ()
  "What country are you from? ")

(defun hs-lang-tags-table-updated ()
  "Oh, I'm sorry, did I break your concentration?")

(defun hs-lang-arbitrary-command-finished ()
  "Oh, you were finished! Well, allow me to retort. What does Marsellus Wallace look like?")

(defun hs-lang-build-done ()
  "Ah, hit the spot.")

(defun hs-lang-build-processing-executables (project)
  (format "My name's %s, and your ass ain't talkin' your way outta this." project))

(defun hs-lang-build-building (project)
  "But I'm tryin', Ringo. I'm tryin' real hard to be the shepherd.")

(defun hs-lang-build-linking (project)
  "I want you to go in that bag, and find my wallet.")

(defun hs-lang-build-compilation-failed (msg)
  (format "I'm sorry, did I break your concentration? %s" msg))

(defun hs-lang-build-compilation-failed-simple (msg)
  (format "I'm sorry, did I break your concentration? I didn't mean to do that. Please, continue, you were saying something about best intentions. What's the matter?"))

(defun hs-lang-load-ok (warnings)
  (if (> warnings 0)
      (format "OK, %d warnings." warnings)
      "OK."))

(defun hs-lang-config-changed ()
  "The Cabal configuration has changed.")

(defun hs-lang-packages-flags-changed-resetting ()
  "That's an interesting point. Come on, let's get into character.")

(defun hs-lang-compiling (module &optional file)
  (format "Buildin' %s%s"
          module
          (if file
              (format " [%s]" file)
            "")))

(defun hs-lang-directory-change (dir)
  (format "I want you to go in that bag, and find my wallet. It's the one that says %s" dir))

(defun hs-lang-directory-change-reload ()
  "The directory was changed. An act of god. Run reload again.")

(defun hs-lang-directory-does-not-exist (dir)
  (format "Say '%s' again. Say 'what' again, I dare you." dir))

(defun hs-lang-command-output ()
  "There's a passage I got memorized. Ezekiel 25:17:")

(defun hs-lang-cabal-ido-command ()
  "What now? ")

(defun hs-lang-cabal-ido-script ()
  "Somethin' 'bout a script? ")

(defun hs-lang-arbitrary-cabal-output ()
  "He said:")

(defun hs-lang-cabal-project-path ()
  "Where we goin'?: ")

(defun hs-lang-cabal-dir (&optional cabal-file)
  (format "You mind if I have some of your tasty beverage to wash this down?%s "
          (if cabal-file
              (format " (%s, right?)" (file-name-nondirectory cabal-file))
            "")))

(defun hs-lang-errors-unused () "Ain't even used")
(defun hs-lang-errors-missing-signature () 
  "Describe what Marsellus Wallace looks like!")
(defun hs-lang-errors-defaulting () 
  "Uuummmm, this is a tasty burger!")
(defun hs-lang-errors-mismatch () "I'm Superfly T.N.T!")
(defun hs-lang-errors-ambiguous () "What?")
(defun hs-lang-errors-illegal () "What country are you from?")
(defun hs-lang-errors-no-instance () "Ain't no instance I've ever heard of")
(defun hs-lang-errors-could-not-deduce () "I ain't deducin' nothin' from this")
(defun hs-lang-errors-warning () "I'm warning you")
(defun hs-lang-errors-error () "Big mistake, Ringo")
(defun hs-lang-errors-incomplete-do () "You were saying? Do what?")
(defun hs-lang-errors-x-against-y (x y)
  (format "“%s”, and “%s” ain't even the same thing." x y))
(defun hs-lang-errors-x-in-y (x y)
  (format "“%s” in “%s”" x y))

(provide 'hs-lang-en)
