;;; hs-config.el — Elisp configurable items.

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

(defun hs-config ())

(defvar hs-config-echo-all nil)

(defvar hs-config-show-filename-in-load-messages nil)

(defvar hs-config-default-project-name "haskell")

(defvar hs-config-cabal-bin-dir "~/.cabal/bin")

(defvar hs-config-use-cabal-dev nil)

(defvar hs-config-cabal-dev-bin (expand-file-name "cabal-dev" hs-config-cabal-bin-dir))

(defvar hs-config-ghci-bin "ghci")

(defvar hs-config-process-prompt-regex "\\(^[> ]*> $\\|\n[> ]*> $\\)")

(defvar hs-config-buffer-prompt "λ> ")

(defvar hs-config-tags-cmd ":!find . -name '*.hs' | xargs hasktags -e -x")

(defvar hs-config-scripts '())

(defvar hs-align-imports-regex
  (concat "^\\(import[ ]+\\)"
          "\\(qualified \\)?"
          "[ ]*\\(\"[^\"]*\" \\)?"
          "[ ]*\\([A-Za-z0-9_.']*\\)"
          "[ ]*\\([ ]*as [A-Z][^ ]*\\)?"
          "[ ]*\\((.*)\\)?"
          "\\([ ]*hiding (.*)\\)?"
          "\\( -- .*\\)?[ ]*$"))

(defvar hs-completion-ghc-extensions
  '("OverlappingInstances" "IncoherentInstances" "UndecidableInstances" "Arrows"
    "ForeignFunctionInterface" "Generics" "ImplicitParams" "ImplicitPrelude"
    "MonomorphismRestriction" "MonoPatBinds" "RelaxedPolyRec" "ExtendedDefaultRules"
    "OverloadedStrings" "GADTs" "TypeFamilies" "ScopedTypeVariables" "TemplateHaskell"
    "QuasiQuotes" "BangPatterns" "CPP" "PatternGuards" "ViewPatterns" "UnicodeSyntax"
    "MagicHash" "NewQualifiedOperators" "PolymorphicComponents" "Rank2Types"
    "RankNTypes" "ImpredicativeTypes" "ExistentialQuantification" "KindSignatures"
    "EmptyDataDecls" "ParallelListComp" "TransformListComp" "UnliftedFFITypes"
    "LiberalTypeSynonyms" "TypeOperators" "RecursiveDo" "PArr" "RecordWildCards"
    "NamedFieldPuns" "DisambiguateRecordFields" "UnboxedTuples" "StandaloneDeriving"
    "DeriveDataTypeable" "GeneralizedNewtypeDeriving" "TypeSynonymInstances"
    "FlexibleContexts" "FlexibleInstances" "ConstrainedClassMethods"
    "MultiParamTypeClasses" "FunctionalDependencies" "PackageImports"))

(defvar hs-completion-ghc-warning-options
  '("-W" "-w" "-w" "-Wall" "-w" "-Werror" "-Wwarn" "-fwarn-unrecognised-pragmas" 
    "-fno-warn-unrecognised-pragmas" "-fwarn-warnings-deprecations"
    "-fno-warn-warnings-deprecations" "-fwarn-deprecated-flags" "-fno-warn-deprecated-flags" 
    "-fwarn-duplicate-exports" "-fno-warn-duplicate-exports" "-fwarn-hi-shadowing" 
    "-fno-warn-hi-shadowing" "-fwarn-implicit-prelude" "-fno-warn-implicit-prelude" 
    "-fwarn-incomplete-patterns" "-fno-warn-incomplete-patterns" 
    "-fwarn-incomplete-record-updates" "-fno-warn-incomplete-record-updates" 
    "-fwarn-missing-fields" "-fno-warn-missing-fields" "-fwarn-missing-methods" 
    "-fno-warn-missing-methods" "-fwarn-missing-signatures" "-fno-warn-missing-signatures" 
    "-fwarn-name-shadowing" "-fno-warn-name-shadowing" "-fwarn-orphans" "-fno-warn-orphans"
    "-fwarn-overlapping-patterns" "-fno-warn-overlapping-patterns" "-fwarn-simple-patterns"
    "-fno-warn-simple-patterns" "-fwarn-tabs" "-fno-warn-tabs" "-fwarn-type-defaults" 
    "-fno-warn-type-defaults" "-fwarn-monomorphism-restriction" 
    "-fno-warn-monomorphism-restriction" "-fwarn-unused-binds" "-fno-warn-unused-binds" 
    "-fwarn-unused-imports" "-fno-warn-unused-imports" "-fwarn-unused-matches" 
    "-fno-warn-unused-matches" "-fwarn-unused-do-bind" "-fno-warn-unused-do-bind" 
    "-fwarn-wrong-do-bind" "-fno-warn-wrong-do-bind"))

(defvar hs-completion-prelude
  '("not" "otherwise" "maybe" "either" "fst" "snd" "curry" "uncurry" "pred"
    "round" "subtract" "odd" "mapM" "mapM_" "sequence" "sequence_" "=<<" "id" 
    "const"
    "flip" "until" "asTypeOf" "error" "undefined" "$!" "seq" "map" "++" "filter"
    "head" "last" "tail" "init" "null" "length" "!!" "reverse" "fold" "fold1"
    "foldr"
    "foldr1" "and" "or" "any" "all" "sum" "product" "concat" "concatMap"
    "maximum"
    "minimum" "scanl" "scanl1" "scanr" "scanr1" "iterate" "repeat" "replicate"
    "cycle" "take" "drop" "splitAt" "takeWhile" "dropWhile" "span" "break" 
    "elem"
    "notElem" "lookup" "zip" "zip3" "zipWith" "zipWith3" "unzip" "unzip3" 
    "lines"
    "words" "unlines" "unwords" "shows" "showChar" "showString" "showParen"
    "reads"
    "readParen" "lex" "putChar" "putStr" "putStrLn" "print" "getChar" 
    "getLine"
    "getContents" "intract" "FilePath" "readFile" "writeFile" "appendFile" 
    "readIO"
    "readLn" "IOException" "ioError" "userError" "catch"))

(defvar hs-completion-prelude-types
  '("Bool" "False" "True" "Char" "String" "IO" "IOError" "Maybe" "Just" "Nothing"
    "Either" "Right" "Left" "Ordering" "LT" "EQ" "GT" "Integer" "Int" "Ratio" 
    "Float" "Double" "Complex"))

(defvar hs-completion-reserved-words
  '("case" "class" "data" "default" "deriving" "do" "else" "if" "import" "in" "infix"
    "infixl" "infixr" "instance" "let" "module" "newtype" "of" "then" "type" "where" 
    "as"
    "qualified" "hiding"))

(provide 'hs-config)
