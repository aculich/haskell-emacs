# ALPHA — don't try to use!

I'm still working on it. See the issue tracker for upcoming features
and bugs. Please wait until test-cases (see “Complete unit testing”
milestone) have been written before contributing patches.

# QUICK START

    (add-to-list 'load-path "~/Emacs/ot/auto-complete")
    (add-to-list 'load-path "~/Emacs/me/hs/src")
    (add-to-list 'auto-mode-alist (cons "\\.hs\\'" 'hs-mode))
    (add-to-list 'auto-mode-alist (cons "\\.cabal\\'" 'hs-cabal-mode))

See examples/bindings.el for example bindings to use. Most of these
will become default eventually.

I will include auto-complete and auto-complete-etags.el (and any other
dependencies) in the distribution in future.

# DONE

* Figuring out project from .cabal file
* Named sessions
* Collapsed/reduced error messages.
* Multi-line expressions
* Cabal build/configure/upload/etc
* Type of symbol at point based on active GHCi session
* Completion based on current module
* Completion based on whole project
* Jump to definition
* Go to error/warning line and column
* In-console completion
* Automatic synchronization with GHCi session (via cabal-dev)
* Sort imports alphabetically
* Align imports up nicely
* Cabal file editing
* Cabal-dev local-repository support
* Language/option/keyword completion
* Move nested blocks of code around
* Jump to/back-from import list for quick editing
* Type of symbol at point (non-local)

# TODO — EASY / TRIVIALLY SPECIFIED

* Indentation that doesn't suck, including indenting/deindenting whole blocks automatically
* GHCi interaction
  * Syntax-highlighted prompt
  * Show-based value inspection
  * Type error handling
  * Module awareness
* Cabal integration
  * Creation! (And magit integration)
  * Configuration
  * Automatic dependency inserting
  * Interactive creation/management of Cabal file
* Source code editing
  * Haskell-aware code-folding
  * Documentation of symbol at point
* Module import-export awareness
  * Completion based on:
   * imported modules
   * installed modules with automatic importation and Cabal-file dependency adding
  * Automatic importation and de-importation of modules for used symbols
  * Hoogle search support
  * Hayoo search support
* Automake/correctness checking
  * Compilation on an interval
  * On-the-fly hint suggestions
* Documentation browsing
  * Ability to browse Haddock documentation inside Emacs (possibility for texinfo here)

# TODO — NOT SURE / TRICKY / NONTRIVIALLY SPECIFIED

* GHCi interaction
  * Debugger tracebacks
* Source code editing
  * Truly syntax-aware editing
  * Binding tracking
  * Syntax-aware indentation choices
  * Inability to write syntactically incorrect code
  * Type of symbol at point (local)

