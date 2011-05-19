# DONE

* Named sessions
* Collapsed/reduced error messages.
* Multi-line expressions
* Cabal build/configure/upload/etc
* Type of symbol at point based on active GHCi session
* Completion based on current module
* Completion based on whole project
* Jump to definition
* Go to error/warning line and column

# TODO

* Projects/Sessions
  * Figuring out project from .cabal file
* GHCi interaction
  * Syntax-highlighted prompt
  * Show-based value inspection
  * Type error handling
  * Debugger tracebacks
  * In-console completion
  * Module awareness
* Cabal integration
  * Configuration
  * Automatic dependency inserting
  * Automatic synchronization with GHCi session
  * Cabal-dev local-repository support
  * Interactive creation/management of Cabal file
* Source code editing
  * Truly syntax-aware editing
  * Binding tracking
  * Syntax-aware indentation choices
  * Inability to write syntactically incorrect code
  * Type of symbol at point
  * Documentation of symbol at point
* module import-export awareness
  * Completion based on:
   * imported modules
   * installed modules with automatic importation and Cabal-file dependency adding
  * Automatic importation and de-importation of modules for used symbols
  * Hoogle search support
* Automake/correctness checking
  * Compilation on an interval
  * On-the-fly hint suggestions
