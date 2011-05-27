;;; hs-indent-test.el — Extensive test-cases for predictable indentation.

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

;; You should M-x hs-indent-test after any changes to the updates,
;; changes that don't pass ALL tests are not acceptable. No
;; exceptions.
;; 
;; If new behaviour is added then the test cases should be updated.

;; An example test error message:

;; Test fail:
;; Test: Indent for `deriving' of data
;; Input:
;; data X
;; |
;; Expected result:
;; data X
;;   |
;; Actual result:
;; data X
;; |
;;
;; This means that the point (‘|’ in this case) is not in the
;; specified place.

;;; Code:

(require 'hs-macros)

(defvar hs-indent-tests
  ;; Identities
  '(((input . "|")
     (name . "Empty line")) 
    ((input . "|foo")
     (name . "Single word, point at start"))
    ((input . "fo|o")
     (name . "Single word, point in middle"))
    ((input . "foo|")
     (name . "Single word, point at end"))
    ((input . "|foo bar mu")
     (name . "Many words, point at start"))
    ((input . "foo| bar mu")
     (name . "Many words, point in middle"))
    ((input . "foo ba|r mu")
     (name . "Many words, point in middle (2)"))
    ((input . "foo bar mu|")
     (name . "Many words, point at end"))
    ((input . "module\n|")
     (name . "Below `module'")) 
    ((input . "module A\n|")
     (name . "Below `module' with name"))
    ((input . "module A where\n|")
     (name . "Below `module' with `where'"))
    ((input . "import X\n|")
     (name . "Below `import'"))
    ((input . "data X deriving Y\n|")
     (name . "Below `deriving'"))
    ((input . "data X\n  deriving Y\n|")
     (name . "Below `deriving' (2)"))
    ((input . "data X\n  |deriving Y")
     (name . "At `deriving'"))
    ((input . "|data X")
     (name . "At `data'"))
    ((input . "|data X\n  deriving Y")
     (name . "At `data' with `deriving'"))
    ((input . "data X\n  |deriving Y")
     (name . "At `deriving' for `data'"))
    ;; Bring-backs
    ((input . "module\n    |")
     (output . "module\n|")
     (name . "Bring back under `module'"))
    ((input . "data X\nderiving Y\n    |")
     (output . "data X\nderiving Y\n|")
     (name . "Bring back under `deriving'"))
    ((input . "data X\n  deriving Y\n    |")
     (output . "data X\n  deriving Y\n|")
     (name . "Bring back under `deriving' with indentation"))
    ((input . "data X = X {\n  foo :: Z\n } deriving Y\n    |")
     (output . "data X = X {\n  foo :: Z\n } deriving Y\n|")
     (name . "Bring back under `deriving' with brace"))
    ((input . "import X\n    |")
     (output . "import X\n|")
     (name . "Bring back under `import'"))
    ((input . "import X\n hiding (Y)\n  |")
     (output . "import X\n hiding (Y)\n|")
     (name . "Bring back under `hiding'"))
    ((input . "import X\n (Y)\n  |")
     (output . "import X\n (Y)\n|")
     (name . "Bring back under explicit symbol import"))
    ;; Keyword-based indents
    ((input . "data X\n|")
     (output . "data X\n  |")
     (name . "Indent for `deriving' of `data'"))
    ;; Keyword-based de-indents
    ((input . "data X\n  |")
     (output . "data X\n|")
     (name . "De-indent for `deriving' of `data'"))
    ;; Definition indentation
    ((input . "main =\n|")
     (output . "main =\n  |")
     (name . "Straight function definition."))
    ))

(defun hs-indent-test ()
  "Run all indentation tests."
  (interactive)
  (mapc 'hs-indent-run-test-spec hs-indent-tests)
  (message (format "Passed %d tests." (length hs-indent-tests))))

(defun hs-indent-run-test-spec (test)
  "Parse and run a test spec."
  (where*
   (hs-indent-run-test buffer input output direction name point)

   ((buffer (get-buffer-create "*hs-indent-tests*"))
    (output (or (cdr (assoc 'output test))
                input))
    (input (cdr (assoc 'input test)))
    (direction (or (cdr (assoc 'direction test))
                   'right))
    (name (or (cdr (assoc 'name test))
              input))
    (point (or (cdr (assoc 'point test))
               "|")))))

(defun hs-indent-run-test (buffer input output direction name point)
  "Run a test."
  (with-current-buffer buffer
    (erase-buffer)
    (insert input)
    (goto-char (point-min))
    (search-forward point)
    (delete-backward-char 1)
    (hs-indent (if (eq direction 'right) 1 -1))
    (insert point)
    (let ((result (buffer-substring-no-properties 
                   (point-min)
                   (point-max))))
      (flet ((report () 
                     (error 
                      (concat 
                       "\n"
                       "Test fail:\n"
                       "Test: %s\n"
                       "Input:\n%s\n"
                       "Expected result:\n%s\n"
                       "Actual result:\n%s\n")
                      name input output result)))
        (when (not (string= result output))
          (report))))))

(provide 'hs-indent-test)