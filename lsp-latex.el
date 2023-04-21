;;; lsp-latex.el --- LSP-mode client for LaTeX, on texlab     -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: languages, tex

;; Version: 3.5.0

;; Package-Requires: ((emacs "26.3") (lsp-mode "6.0"))
;; URL: https://github.com/ROCKTAKEY/lsp-latex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Table of Contents
;; _________________

;; 1. lsp-mode client for texlab.
;; 2. How to Use?
;; 3. Variables
;; .. 1. `lsp-latex-texlab-executable'
;; .. 2. `lsp-latex-texlab-executable-argument-list'
;; .. 3. Others, provided by texlab server
;; 4. Build
;; .. 1. `lsp-latex-build'
;; 5. Workspace commands
;; .. 1. `lsp-latex-clean-auxiliary'
;; .. 2. `lsp-latex-clean-artifacts'
;; .. 3. `lsp-latex-change-environment'
;; 6. Forward/inverse search
;; .. 1. Forward search
;; .. 2. Inverse search
;; .. 3. Examples
;; ..... 1. SumatraPDF
;; ..... 2. Evince
;; ..... 3. Okular
;; ..... 4. Zathura
;; ..... 5. qpdfview
;; ..... 6. Skim
;; ..... 7. `pdf-tools' integration
;; 7. License


;; [https://img.shields.io/github/tag/ROCKTAKEY/lsp-latex.svg?style=flat-square]
;; [https://img.shields.io/github/license/ROCKTAKEY/lsp-latex.svg?style=flat-square]
;; [https://img.shields.io/github/actions/workflow/status/ROCKTAKEY/lsp-latex/CI.yml.svg?style=flat-square]
;; [file:https://melpa.org/packages/lsp-latex-badge.svg]


;; [https://img.shields.io/github/tag/ROCKTAKEY/lsp-latex.svg?style=flat-square]
;; <https://github.com/ROCKTAKEY/lsp-latex>

;; [https://img.shields.io/github/license/ROCKTAKEY/lsp-latex.svg?style=flat-square]
;; <file:LICENSE>

;; [https://img.shields.io/github/actions/workflow/status/ROCKTAKEY/lsp-latex/CI.yml.svg?style=flat-square]
;; <https://github.com/ROCKTAKEY/lsp-latex/actions>

;; [file:https://melpa.org/packages/lsp-latex-badge.svg]
;; <https://melpa.org/#/lsp-latex>


;; 1 lsp-mode client for texlab.
;; =============================

;;   While `lsp-tex.el', included by [lsp-mode], provides minimal setting
;;   for [texlab], `lsp-latex.el' provides full features of [texlab]!


;; [lsp-mode] <https://github.com/emacs-lsp/lsp-mode>

;; [texlab] <https://github.com/latex-lsp/texlab>


;; 2 How to Use?
;; =============

;;   - First, you have to install `texlab'.  Please install this [here].
;;   - Next, you should make `lsp-mode' available.  See [lsp-mode].
;;   - Now, you can use Language Server Protocol (LSP) on (la)tex-mode or
;;     yatex-mode just to evaluate this:

;;   ,----
;;   |  1  (add-to-list 'load-path "/path/to/lsp-latex")
;;   |  2  (require 'lsp-latex)
;;   |  3  ;; "texlab" must be located at a directory contained in `exec-path'.
;;   |  4  ;; If you want to put "texlab" somewhere else,
;;   |  5  ;; you can specify the path to "texlab" as follows:
;;   |  6  ;; (setq lsp-latex-texlab-executable "/path/to/texlab")
;;   |  7
;;   |  8  (with-eval-after-load "tex-mode"
;;   |  9   (add-hook 'tex-mode-hook 'lsp)
;;   | 10   (add-hook 'latex-mode-hook 'lsp))
;;   | 11
;;   | 12  ;; For YaTeX
;;   | 13  (with-eval-after-load "yatex"
;;   | 14   (add-hook 'yatex-mode-hook 'lsp))
;;   | 15
;;   | 16  ;; For bibtex
;;   | 17  (with-eval-after-load "bibtex"
;;   | 18   (add-hook 'bibtex-mode-hook 'lsp))
;;   `----


;; [here] <https://github.com/latex-lsp/texlab/releases>

;; [lsp-mode] <https://github.com/emacs-lsp/lsp-mode>


;; 3 Variables
;; ===========

;; 3.1 `lsp-latex-texlab-executable'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Where texlab server located.


;; 3.2 `lsp-latex-texlab-executable-argument-list'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Argument list passed to texlab server.


;; 3.3 Others, provided by texlab server
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   These variables are connected to texlab configuration variables.  See
;;   also [texlab official wiki].
;;    Custom variable in Emacs                      Configuration provided by texlab
;;   ----------------------------------------------------------------------------------------
;;    lsp-latex-root-directory                      texlab.rootDirectory
;;    lsp-latex-build-executable                    texlab.build.executable
;;    lsp-latex-build-args                          texlab.build.args
;;    lsp-latex-build-forward-search-after          texlab.build.forwardSearchAfter
;;    lsp-latex-build-on-save                       texlab.build.onSave
;;    lsp-latex-build-aux-directory                 texlab.auxDirectory
;;    lsp-latex-forward-search-executable           texlab.forwardSearch.executable
;;    lsp-latex-forward-search-args                 texlab.forwardSearch.args
;;    lsp-latex-chktex-on-open-and-save             texlab.chktex.onOpenAndSave
;;    lsp-latex-chktex-on-edit                      texlab.chktex.onEdit
;;    lsp-latex-diagnostics-delay                   texlab.diagnosticsDelay
;;    lsp-latex-diagnostics-allowed-patterns        texlab.diagnostics.allowedPatterns
;;    lsp-latex-diagnostics-ignored-patterns        texlab.diagnostics.ignoredPatterns
;;    lsp-latex-symbol-allowed-patterns             texlab.symbol.allowedPatterns
;;    lsp-latex-symbol-ignored-patterns             texlab.symbol.ignoredPatterns
;;    lsp-latex-bibtex-formatter-line-length        texlab.formatterLineLength
;;    lsp-latex-bibtex-formatter                    texlab.bibtexFormatter
;;    lsp-latex-latex-formatter                     texlab.latexFormatter
;;    lsp-latex-latexindent-local                   texlab.latexindent.local
;;    lsp-latex-latexindent-modify-line-breaks      texlab.latexindent.modifyLineBreaks
;;    lsp-latex-completion-matcher                  texlab.completion.matcher
;;    lsp-latex-experimental-math-environments      texlab.experimental.mathEnvironments
;;    lsp-latex-experimental-enum-environments      texlab.experimental.enumEnvironments
;;    lsp-latex-experimental-verbatim-environments  texlab.experimental.verbatimEnvironments
;;    lsp-latex-experimental-citation-commands      texlab.experimental.citationCommands


;; [texlab official wiki]
;; <https://github.com/latex-lsp/texlab/wiki/Configuration>


;; 4 Build
;; =======

;; 4.1 `lsp-latex-build'
;; ~~~~~~~~~~~~~~~~~~~~~

;;   Build .tex files with texlab.  It use latexmk by default, so add
;;   .latexmkrc if you want to customize latex commands or options.  You can
;;   change build command and option to other such as `make`, by changing
;;   `lsp-latex-build-executable' and `lsp-latex-build-args'.

;;   This command build asynchronously by default, while it build
;;   synchronously with prefix argument(C-u).


;; 5 Workspace commands
;; ====================

;;   See also [texlab official wiki].


;; [texlab official wiki]
;; <https://github.com/latex-lsp/texlab/wiki/Workspace-commands>

;; 5.1 `lsp-latex-clean-auxiliary'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   This command removes LaTeX auxiliary files.  It will run `latexmk -c'
;;   in the project.


;; 5.2 `lsp-latex-clean-artifacts'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   This command removes LaTeX auxiliary files and artifacts It will run
;;   `latexmk -C' in the project.


;; 5.3 `lsp-latex-change-environment'
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   This command replaces enviroment name to NEW-NAME in current position.
;;   This edits most-inner environment containing the current position.


;; 6 Forward/inverse search
;; ========================

;;   Forward search and inverse search are available.  See also [texlab
;;   official wiki].


;; [texlab official wiki]
;; <https://github.com/latex-lsp/texlab/wiki/Previewing>

;; 6.1 Forward search
;; ~~~~~~~~~~~~~~~~~~

;;   You can move from Emacs to current position on pdf viewer by the
;;   function `lsp-latex-forward-search'.  To use, you should set
;;   `lsp-latex-forward-search-executable' and
;;   `lsp-latex-forward-search-args' according to your pdf viewer.

;;   You can see [texlab official wiki], but you should replace some VSCode
;;   words with Emacs words.  `latex.forwardSearch.executable' should be
;;   replaced with `lsp-latex-forward-search-executable', and
;;   `latex.forwardSearch.args' with `lsp-latex-forward-search-args'.  You
;;   should setq each variable instead of writing like json, and vector in
;;   json is replaced to list in Emacs Lisp.  So the json:
;;   ,----
;;   | {
;;   |        "texlab.forwardSearch.executable": "FavoriteViewer",
;;   |        "texlab.forwardSearch.args": [ "%p", "%f", "%l" ]
;;   | }
;;   `----
;;   should be replaced with the Emacs Lisp code:
;;   ,----
;;   | (setq lsp-latex-forward-search-executable "FavoriteViewer")
;;   | (setq lsp-latex-forward-search-args '("%p" "%f" "%l"))
;;   `----

;;   In `lsp-latex-forward-search-args', the string "%f" is replaced with
;;   "The path of the current TeX file", "%p" with "The path of the current
;;   PDF file", "%l" with "The current line number", by texlab (see
;;   [here]).

;;   For example of SumatraPDF, write in init.el:
;;   ,----
;;   | (setq lsp-latex-forward-search-executable "C:/Users/{User}/AppData/Local/SumatraPDF/SumatraPDF.exe")
;;   | (setq lsp-latex-forward-search-args '("-reuse-instance" "%p" "-forward-search" "%f" "%l"))
;;   `----
;;   while VSCode config with json (see [texlab official wiki]) is:
;;   ,----
;;   | {
;;   |   "texlab.forwardSearch.executable": "C:/Users/{User}/AppData/Local/SumatraPDF/SumatraPDF.exe",
;;   |   "texlab.forwardSearch.args": [
;;   |     "-reuse-instance",
;;   |     "%p",
;;   |     "-forward-search",
;;   |     "%f",
;;   |     "%l"
;;   |   ]
;;   | }
;;   `----

;;   Then, you can jump to the current position on pdf viewer by command
;;   `lsp-latex-forward-search'.


;; [texlab official wiki]
;; <https://github.com/latex-lsp/texlab/wiki/Previewing>

;; [here]
;; <https://github.com/latex-lsp/texlab/wiki/Configuration#texlabforwardsearchargs>

;; [texlab official wiki]
;; <https://github.com/latex-lsp/texlab/wiki/Previewing#forward-search>


;; 6.2 Inverse search
;; ~~~~~~~~~~~~~~~~~~

;;   You can go to the current position on Emacs from pdf viewer.  Whatever
;;   pdf viewer you use, you should start Emacs server by writing in
;;   init.el:
;;   ,----
;;   | (server-start)
;;   `----
;;   Then, you can jump to line {{LINE-NUMBER}} in file named {{FILENAME}}
;;   with the command:
;;   ,----
;;   | 1  emacsclient +{{LINE-NUMBER}} {{FILENAME}}
;;   `----
;;   {{LINE-NUMBER}} and {{FILENAME}} should be replaced with line number
;;   and filename you want to jump to.  Each pdf viewer can provide some
;;   syntax to replace.

;;   For example of SmatraPDF (see [texlab official wiki]), "Add the
;;   following line to your SumatraPDF settings file (Menu -> Settings ->
;;   Advanced Options):"
;;   ,----
;;   | 1  InverseSearchCmdLine = C:\path\to\emacsclient.exe +%l %f
;;   `----
;;   Then, "You can execute the search by pressing Alt+DoubleClick in the
;;   PDF document".


;; [texlab official wiki]
;; <https://github.com/latex-lsp/texlab/wiki/Previewing#inverse-search>


;; 6.3 Examples
;; ~~~~~~~~~~~~

;;   These examples are according to [texlab official wiki].  Especially,
;;   quoted or double-quoted sentences are citation from [texlab official
;;   wiki].


;; [texlab official wiki]
;; <https://github.com/latex-lsp/texlab/wiki/Previewing>

;; 6.3.1 SumatraPDF
;; ----------------

;;         We highly recommend SumatraPDF on Windows because Adobe
;;         Reader locks the opened PDF file and will therefore
;;         prevent further builds.


;; * 6.3.1.1 Forward search

;;   Write to init.el:
;;   ,----
;;   | (setq lsp-latex-forward-search-executable "C:/Users/{User}/AppData/Local/SumatraPDF/SumatraPDF.exe")
;;   | (setq lsp-latex-forward-search-args '("-reuse-instance" "%p" "-forward-search" "%f" "%l"))
;;   `----


;; * 6.3.1.2 Inverse Search

;;         Add the following line to your [SumatraPDF] settings file
;;         (Menu -> Settings -> Advanced Options):
;;   ,----
;;   | 1  InverseSearchCmdLine = C:\path\to\emacsclient.exe +%l "%f"
;;   `----
;;         You can execute the search by pressing `Alt+DoubleClick'
;;         in the PDF document.


;;   [SumatraPDF] <https://www.sumatrapdfreader.org/>


;; 6.3.2 Evince
;; ------------

;;         The SyncTeX feature of [Evince] requires communication via
;;         D-Bus.  In order to use it from the command line, install
;;         the [evince-synctex] script.


;; [Evince] <https://wiki.gnome.org/Apps/Evince>

;; [evince-synctex] <https://github.com/latex-lsp/evince-synctex>

;; * 6.3.2.1 Forward search

;;   Write to init.el:
;;   ,----
;;   | (setq lsp-latex-forward-search-executable "evince-synctex")
;;   | (setq lsp-latex-forward-search-args '("-f" "%l" "%p" "\"emacsclient +%l %f\""))
;;   `----


;; * 6.3.2.2 Inverse search

;;         The inverse search feature is already configured if you
;;         use `evince-synctex'.  You can execute the search by
;;         pressing `Ctrl+Click' in the PDF document.


;; 6.3.3 Okular
;; ------------

;; * 6.3.3.1 Forward search

;;   Write to init.el:
;;   ,----
;;   | (setq lsp-latex-forward-search-executable "okular")
;;   | (setq lsp-latex-forward-search-args '("--unique" "file:%p#src:%l%f"))
;;   `----


;; * 6.3.3.2 Inverse search

;;         Change the editor of Okular (Settings -> Configure
;;         Okular... -> Editor) to "Custom Text Editor" and set the
;;         following command:
;;   ,----
;;   | emacsclient +%l "%f"
;;   `----
;;   You can execute the search by pressing `Shift+Click' in the PDF
;;   document.


;; 6.3.4 Zathura
;; -------------

;; * 6.3.4.1 Forward search

;;   Write to init.el:
;;   ,----
;;   | (setq lsp-latex-forward-search-executable "zathura")
;;   | (setq lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p"))
;;   `----


;; * 6.3.4.2 Inverse search

;;         Add the following lines to your
;;         `~/.config/zathura/zathurarc' file:
;;   ,----
;;   | 1  set synctex true
;;   | 2  set synctex-editor-command "emacsclient +%{line} %{input}"
;;   `----
;;         You can execute the search by pressing `Alt+Click' in the
;;         PDF document.


;; 6.3.5 qpdfview
;; --------------

;; * 6.3.5.1 Forward search

;;   Write to init.el:
;;   ,----
;;   | (setq lsp-latex-forward-search-executable "qpdfview")
;;   | (setq lsp-latex-forward-search-args '("--unique" "%p#src:%f:%l:1"))
;;   `----


;; * 6.3.5.2 Inverse search

;;         Change the source editor setting (Edit -> Settings... ->
;;         Behavior -> Source editor) to:
;;   ,----
;;   | 1  emacsclient +%2 "%1"
;;   `----
;;         and select a mouse button modifier (Edit -> Settings... ->
;;         Behavior -> Modifiers -> Mouse button modifiers -> Open in
;;         Source Editor)of choice.  You can execute the search by
;;         pressing Modifier+Click in the PDF document.


;; 6.3.6 Skim
;; ----------

;;         We recommend [Skim] on macOS since it is the only native
;;         viewer that supports SyncTeX.  Additionally, enable the
;;         "Reload automatically" setting in the Skim preferences
;;         (Skim -> Preferences -> Sync -> Check for file changes).


;; [Skim] <https://skim-app.sourceforge.io/>

;; * 6.3.6.1 Forward search

;;   Write to init.el:
;;   ,----
;;   | (setq lsp-latex-forward-search-executable "/Applications/Skim.app/Contents/SharedSupport/displayline")
;;   | (setq lsp-latex-forward-search-args '("%l" "%p" "%f"))
;;   `----
;;   "If you want Skim to stay in the background after executing the
;;   forward search, you can add the `-g' option to"
;;   `lsp-latex-forward-search-args'.


;; * 6.3.6.2 Inverse search

;;   Select Emacs preset "in the Skim preferences (Skim -> Preferences ->
;;   Sync -> PDF-TeX Sync support).  You can execute the search by pressing
;;   `Shift+⌘+Click' in the PDF document."


;; 6.3.7 `pdf-tools' integration
;; -----------------------------

;;   If you want to use forward search with `pdf-tools', follow the
;;   setting:
;;   ,----
;;   | ;; Start Emacs server
;;   | (server-start)
;;   | ;; Turn on SyncTeX on the build.
;;   | ;; If you use `lsp-latex-build', it is on by default.
;;   | ;; If not (for example, YaTeX or LaTeX-mode building system),
;;   | ;; put to init.el like this:
;;   | (setq tex-command "platex --synctex=1")
;;   |
;;   | ;; Setting for pdf-tools
;;   | (setq lsp-latex-forward-search-executable "emacsclient")
;;   | (setq lsp-latex-forward-search-args
;;   |       '("--eval"
;;   |         "(lsp-latex-forward-search-with-pdf-tools \"%f\" \"%p\" \"%l\")"))
;;   `----
;;   Inverse research is not provided by texlab, so please use
;;   `pdf-sync-backward-search-mouse'.


;; 7 License
;; =========

;;   This package is licensed by GPLv3. See [LICENSE].


;; [LICENSE] <file:LICENSE>

;;; Code:
(require 'lsp-mode)
(require 'cl-lib)

(defgroup lsp-latex nil
  "Language Server Protocol client for LaTeX."
  :group 'lsp-mode)



(defcustom lsp-latex-texlab-executable
  (cond ((eq system-type 'windows-nt)
         "texlab.exe")
        (t "texlab"))
  "Executable command to run texlab.
Called with the arguments in `lsp-latex-texlab-executable-argument-list'."
  :group 'lsp-latex
  :type 'string)

(defcustom lsp-latex-texlab-executable-argument-list '()
  "List of Arguments passed to `lsp-latex-texlab-executable'."
  :group 'lsp-latex
  :type '(repeat string))



(defcustom lsp-latex-completion-sort-in-emacs nil
  "Sort completion results in Emacs if non-nil.
Texlab sorts completion results, so sorting in Emacs is not needed.
Set non-nil value if you prefers they are sorted in Emacs.
See also `lsp-completion-sort-initial-results'."
  :group 'lsp-latex
  :type 'boolean
  :version "3.2.0")



(defcustom lsp-latex-root-directory nil
  "Root directory of each buffer."
  :group 'lsp-latex
  :risky t
  :type '(choice string
                 (const nil)))

(defcustom lsp-latex-build-executable "latexmk"
  "Build command used on `lsp-latex-build'."
  :group 'lsp-latex
  :risky t
  :type 'string)

(defcustom lsp-latex-build-args
  '("-pdf" "-interaction=nonstopmode" "-synctex=1" "%f")
  "Argument list passed to `lsp-latex-build-executable'.
Value is used on `lsp-latex-build'.
\"%f\" can be used as the path of the TeX file to compile."
  :group 'lsp-latex
  :risky t
  :type '(repeat string))

(define-obsolete-variable-alias 'lsp-latex-forward-search-after
  'lsp-latex-build-forward-search-after
   "3.0.0")

(defcustom lsp-latex-build-forward-search-after nil
  "Execute forward-research after building."
  :group 'lsp-latex
  :type 'boolean
  :version "3.0.0")

(defcustom lsp-latex-build-on-save nil
  "Build after saving a file or not."
  :group 'lsp-latex
  :type 'boolean
  :version "3.0.0")

(define-obsolete-variable-alias 'lsp-latex-build-output-directory
  'lsp-latex-build-aux-directory
  "2.0.0"
  "Directory to which built file is put.
Note that you should change `lsp-latex-build-args' to change output directory.
If you use latexmk, use \"-outdir\" flag.

This variable is obsoleted since texlab 3.0.0.")

(defcustom lsp-latex-build-aux-directory "."
  "Directory to which built file is put.
Note that you should change `lsp-latex-build-args' to change output directory.
If you use latexmk, use \"-outdir\" flag."
  :group 'lsp-latex
  :type 'string
  :risky t
  :version "2.0.0")

(make-obsolete-variable
 'lsp-latex-build-is-continuous
 "This variable is obsoleted since texlab 3.2.0.
https://github.com/latex-lsp/texlab/blob/fe828eed914088c6ad90a4574192024008b3d96a/CHANGELOG.md#changed."
 "3.0.0")

(defcustom lsp-latex-forward-search-executable nil
  "Executable command used to search in preview.
It is passed server as \"latex.forwardSearch.executable\"."
  :group 'lsp-latex
  :type 'string
  :risky t)

(defcustom lsp-latex-forward-search-args nil
  "Argument list passed to `lsp-latex-forward-search-executable'.
It is passed server as \"latex.forwardSearch.executable\".

Placeholders
    %f: The path of the current TeX file.
    %p: The path of the current PDF file.
    %l: The current line number."
  :group 'lsp-latex
  :type '(repeat string)
  :risky t)

(define-obsolete-variable-alias 'lsp-latex-lint-on-save
  'lsp-latex-chktex-on-open-and-save
   "2.0.0"
   "Lint using chktex after saving a file.

This variable is obsoleted since texlab 3.0.0.")

(defcustom lsp-latex-chktex-on-open-and-save nil
  "Lint using chktex after opening and saving a file."
  :group 'lsp-latex
  :type 'boolean
  :version "2.0.0")

(define-obsolete-variable-alias 'lsp-latex-lint-on-change
  'lsp-latex-chktex-on-edit
  "2.0.0"
  "Lint using chktex after changing a file.

This variable is obsoleted since texlab 3.0.0.")

(defcustom lsp-latex-chktex-on-edit nil
  "Lint using chktex after changing a file."
  :group 'lsp-latex
  :type 'boolean
  :version "2.0.0")

(defcustom lsp-latex-diagnostics-delay 300
  "Delay time before reporting diagnostics.
The value is in milliseconds."
  :group 'lsp-latex
  :type 'integerp
  :version "2.0.0")

(defcustom lsp-latex-diagnostics-allowed-patterns '()
  "Regexp whitelist for diagnostics.
It should be a list of regular expression.
Only diagnostics that match at least one of the elemnt is shown.

Note that this is applied before `lsp-latex-diagnostics-ignored-patterns',
so `lsp-latex-diagnostics-ignored-patterns' is priored."
  :group 'lsp-latex
  :type '(repeat string)
  :version "3.0.0")

(defcustom lsp-latex-diagnostics-ignored-patterns '()
  "Regexp blacklist for diagnostics.
It should be a list of regular expression.
Only diagnostics that do NOT match at least one of the elemnt is shown.

Note that this is applied after `lsp-latex-diagnostics-allowed-patterns',
so this variable is priored."
  :group 'lsp-latex
  :type '(repeat string)
  :version "3.0.0")

(defcustom lsp-latex-symbol-allowed-patterns '()
  "Regexp whitelist for document symbol.
It should be a list of regular expression.
Only document symbol that match at least one of the elemnt is shown.

Note that this is applied before `lsp-latex-symbol-ignored-patterns',
so `lsp-latex-symbol-ignored-patterns' is priored."
  :group 'lsp-latex
  :type '(repeat string)
  :version "3.5.0")

(defcustom lsp-latex-symbol-ignored-patterns '()
  "Regexp blacklist for document symbol.
It should be a list of regular expression.
Only document symbol that do NOT match at least one of the elemnt is shown.

Note that this is applied after `lsp-latex-symbol-allowed-patterns',
so this variable is priored."
  :group 'lsp-latex
  :type '(repeat string)
  :version "3.5.0")

(define-obsolete-variable-alias 'lsp-latex-bibtex-formatting-line-length
  'lsp-latex-bibtex-formatter-line-length
  "Maximum amount of line on formatting BibTeX files.
0 means disable.

This variable is obsoleted since texlab 3.0.0.")

(defcustom lsp-latex-bibtex-formatter-line-length 80
  "Maximum amount of line on formatting BibTeX files.
0 means disable."
  :group 'lsp-latex
  :type 'integerp
  :version "2.0.0")

(define-obsolete-variable-alias 'lsp-latex-bibtex-formatting-formatter
  'lsp-latex-bibtex-formatter
  "2.0.0"
  "Formatter used to format BibTeX file.
You can choose \"texlab\" or \"latexindent\".

This variable is obsoleted since texlab 3.0.0.")

(defcustom lsp-latex-bibtex-formatter "texlab"
  "Formatter used to format BibTeX file.
You can choose \"texlab\" or \"latexindent\"."
  :group 'lsp-latex
  :type '(choice (const "texlab") (const "latexindent"))
  :version "2.0.0")

(defcustom lsp-latex-latex-formatter "texlab"
  "Formatter used to format LaTeX file.
You can choose \"texlab\" or \"latexindent\".

This variable is valid since texlab 3.0.0."
  :group 'lsp-latex
  :type '(choice (const "texlab") (const "latexindent"))
  :version "2.0.0")

(defcustom lsp-latex-latexindent-local nil
  "Path to file of latexindent configuration.
The value is passed to latexindent through \"--local\" flag.
The root directory is used by default."
  :group 'lsp-latex
  :type '(choice string (const nil))
  :version "2.0.0")

(defcustom lsp-latex-latexindent-modify-line-breaks nil
  "Latexindent modifies line breaks if t."
  :group 'lsp-latex
  :type 'boolean
  :version "2.0.0")

(defcustom lsp-latex-completion-matcher "fuzzy-ignore-case"
  "Algorithm used to filter the completion by texlab.
\"fuzzy\", which means fuzzy matching, or \"prefix\",
which means that prefix matching is allowed.
In addition, \"-ignore-case\" suffix is also available,
which means the matcher should be case insensitive."
  :group 'lsp-latex
  :type '(choice (const "fuzzy")
                 (const "fuzzy-ignore-case")
                 (const "prefix")
                 (const "prefix-ignore-case"))
  :version "3.5.0")

(defcustom lsp-latex-experimental-math-environments '()
  "List of environment name regarded as math environment, such as \"align\"."
  :group 'lsp-latex
  :type '(repeat string)
  :version "3.3.0")

(defcustom lsp-latex-experimental-enum-environments '()
  "List of environment name regarded as enumelation environment.
For example, \"itemize\" or \"enumerate\" meet the condition."
  :group 'lsp-latex
  :type '(repeat string)
  :version "3.3.0")

(defcustom lsp-latex-experimental-verbatim-environments '()
  "List of environment name regarded as verbatim environment.
This suppresses warnings in the environment where the code is not
written in LaTeX.
For example, \"lstlisting\" is meet the condition."
  :group 'lsp-latex
  :type '(repeat string)
  :version "3.3.0")

(defcustom lsp-latex-experimental-citation-commands '()
  "List of command name which should be regarded as citation command.
For example, \"cite\" is meet the condition. Note that backslash is not needed."
  :group 'lsp-latex
  :type '(repeat string)
  :version "3.5.0")

(defun lsp-latex--getter-vectorize-list (symbol)
  "Make list in SYMBOL into vector.
This function is thoughted to be used with `apply-partially'.

This function is used for the treatment before `json-serialize',
because `json-serialize' cannot recognize normal list as array of json."
  (vconcat (eval symbol)))

(defun lsp-latex--diagnostics-allowed-patterns ()
  "Get `lsp-latex-build-args' with changing to vector.
Because `json-serialize' cannot recognize normal list as array of json,
should be vector."
  (vconcat lsp-latex-build-args))

(defun lsp-latex-setup-variables ()
  "Register texlab customization variables to function `lsp-mode'."
  (interactive)
  (lsp-register-custom-settings
   `(("texlab.rootDirectory" lsp-latex-root-directory)
     ("texlab.build.executable" lsp-latex-build-executable)
     ("texlab.build.args" ,(apply-partially #'lsp-latex--getter-vectorize-list 'lsp-latex-build-args))
     ("texlab.build.forwardSearchAfter" lsp-latex-build-forward-search-after t)
     ("texlab.build.onSave" lsp-latex-build-on-save t)
     ("texlab.auxDirectory" lsp-latex-build-aux-directory)
     ("texlab.forwardSearch.executable" lsp-latex-forward-search-executable)
     ("texlab.forwardSearch.args" ,(apply-partially #'lsp-latex--getter-vectorize-list 'lsp-latex-forward-search-args))
     ("texlab.chktex.onOpenAndSave" lsp-latex-chktex-on-open-and-save t)
     ("texlab.chktex.onEdit" lsp-latex-chktex-on-edit t)
     ("texlab.diagnosticsDelay" lsp-latex-diagnostics-delay)
     ("texlab.diagnostics.allowedPatterns" ,(apply-partially #'lsp-latex--getter-vectorize-list 'lsp-latex-diagnostics-allowed-patterns))
     ("texlab.diagnostics.ignoredPatterns" ,(apply-partially #'lsp-latex--getter-vectorize-list 'lsp-latex-diagnostics-ignored-patterns))
     ("texlab.formatterLineLength" lsp-latex-bibtex-formatter-line-length)
     ("texlab.bibtexFormatter" lsp-latex-bibtex-formatter)
     ("texlab.latexFormatter" lsp-latex-latex-formatter)
     ("texlab.latexindent.local" lsp-latex-latexindent-local)
     ("texlab.latexindent.modifyLineBreaks" lsp-latex-latexindent-modify-line-breaks)
     ("texlab.completion.matcher" lsp-latex-completion-matcher)
     ("texlab.experimental.mathEnvironments" lsp-latex-experimental-math-environments)
     ("texlab.experimental.enumEnvironments"lsp-latex-experimental-enum-environments)
     ("texlab.experimental.verbatimEnvironments" lsp-latex-experimental-verbatim-environments)
     ("texlab.experimental.citationCommands" lsp-latex-experimental-citation-commands))))

(lsp-latex-setup-variables)

(add-to-list 'lsp-language-id-configuration '(".*\\.tex$" . "latex"))
(add-to-list 'lsp-language-id-configuration '(".*\\.bib$" . "bibtex"))

(defun lsp-latex-new-connection ()
  "Create new connection of lsp-latex."
  (if (locate-file lsp-latex-texlab-executable exec-path)
      (cons lsp-latex-texlab-executable
            lsp-latex-texlab-executable-argument-list)
    (error "No executable \"texlab\" file")))

;; Copied from `lsp-clients--rust-window-progress' in `lsp-rust'.
(defun lsp-latex-window-progress (_workspace params)
  "Progress report handling.
PARAMS progress report notification data."
  ;; Minimal implementation - we could show the progress as well.
  (lsp-log (gethash "title" params)))

(lsp-register-client
 (make-lsp-client :new-connection
                  (lsp-stdio-connection
                   #'lsp-latex-new-connection)
                  :major-modes '(tex-mode
                                 yatex-mode
                                 latex-mode
                                 bibtex-mode)
                  :server-id 'texlab2
                  :priority 2
                  :initialized-fn
                  (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       (lsp-configuration-section "latex"))
                      (lsp--set-configuration
                       (lsp-configuration-section "bibtex"))))
                  :notification-handlers
                  (lsp-ht
                   ("window/progress"
                    'lsp-latex-window-progress))
                  :after-open-fn
                  (lambda ()
                    (setq-local lsp-completion-sort-initial-results lsp-latex-completion-sort-in-emacs))))


;;; Build

(defun lsp-latex--message-result-build (result)
  "Message RESULT means success or not."
  (message
   (cl-case (gethash "status" result)
     ((0)                             ;Success
      "Build succeeded.")
     ((1)                             ;Error
      "Build error.")
     ((2)                             ;Failure
      "Build failed.")
     ((3)                             ;Cancelled
      "Build cancelled."))))

(defun lsp-latex-build (&optional sync)
  "Build current tex file with latexmk, through texlab.
Build synchronously if SYNC is non-nil."
  (interactive "P")
  (if sync
      (lsp-latex--message-result-build
       (lsp-request
       "textDocument/build"
       (list :textDocument (lsp--text-document-identifier))))
    (lsp-request-async
     "textDocument/build"
     (list :textDocument (lsp--text-document-identifier))
     #'lsp-latex--message-result-build)))


;;; Forward search

;; To suppress warning.
(defvar pdf-sync-forward-display-action)
(declare-function pdf-info-synctex-forward-search "ext:pdf-info")
(declare-function pdf-sync-synctex-file-name "ext:pdf-sync")
(declare-function pdf-util-assert-pdf-window "ext:pdf-util")
(declare-function pdf-util-tooltip-arrow "ext:pdf-util")
(declare-function pdf-view-goto-page "ext:pdf-view")
(declare-function pdf-view-image-size "ext:pdf-view")

;;;###autoload
(defun lsp-latex-forward-search-with-pdf-tools (tex-file pdf-file line)
  "Forward search with pdf-tools, from TEX-FILE line LINE to PDF-FILE.
This function is partially copied from
`pdf-sync-forward-search' and `pdf-sync-forward-correlate'."
  (unless (fboundp 'pdf-tools-install)
    (error "Please install pdf-tools"))
  (require 'pdf-tools)
  (require 'pdf-sync)

  (with-current-buffer (get-file-buffer tex-file)
   (cl-destructuring-bind (pdf page _x1 y1 _x2 _y2)
       (let* ((column 1)
              (pdf (expand-file-name (with-no-warnings pdf-file)))
              (sfilename (pdf-sync-synctex-file-name
                          (buffer-file-name) pdf)))
         (cons pdf
               (condition-case error
                   (let-alist (pdf-info-synctex-forward-search
                               (or sfilename
                                   (buffer-file-name))
                               line column pdf)
                     (cons .page .edges))
                 (error
                  (message "%s" (error-message-string error))
                  (list nil nil nil nil nil)))))
     (let ((buffer (or (find-buffer-visiting pdf)
                       (find-file-noselect pdf))))
       (with-selected-window (display-buffer
                              buffer pdf-sync-forward-display-action)
         (pdf-util-assert-pdf-window)
         (when page
           (pdf-view-goto-page page)
           (when y1
             (let ((top (* y1 (cdr (pdf-view-image-size)))))
               (pdf-util-tooltip-arrow (round top))))))
       (with-current-buffer buffer
         (run-hooks 'pdf-sync-forward-hook))))))

(defun lsp-latex--message-forward-search (result)
  "Message unless RESULT means success."
  (message
   (cl-case (plist-get result :status)
     ((1)                             ;Error
      "Forward search do not succeeded.")
     ((2)                             ;Failure
      "Forward search failed.")
     ((3)                             ;Unconfigured
      "Forward search has not been configured."))))

(defun lsp-latex-forward-search ()
  "Forward search on preview."
  (interactive)
  (lsp-request-async
   "textDocument/forwardSearch"
   (lsp--text-document-position-params)
   #'lsp-latex--message-forward-search))


;;; Workspace commands

(defun lsp-latex-clean-auxiliary (text-document-identifier)
  "Remove LaTeX auxiliary files.
Auxiliary files in project specified by TEXT-DOCUMENT-IDENTIFIER is removed.
It will run \"latexmk -c\" in the project.

When called interactively, TEXT-DOCUMENT-IDENTIFIER is provided by
`lsp-text-document-identifier'."
  (interactive
   (list (lsp-text-document-identifier)))
  (lsp-workspace-command-execute "texlab.cleanAuxiliary"
                                 (vector text-document-identifier)))

(defun lsp-latex-clean-artifacts (text-document-identifier)
  "Remove LaTeX auxiliary files and artifacts.
Removed files are in project specified by TEXT-DOCUMENT-IDENTIFIER.
It will run \"latexmk -C\" in the project.

When called interactively, TEXT-DOCUMENT-IDENTIFIER is provided by
`lsp-text-document-identifier'."
  (interactive
   (list (lsp-text-document-identifier)))
  (lsp-workspace-command-execute "texlab.cleanArtifacts"
                                 (vector text-document-identifier)))

(defun lsp-latex-change-environment (text-document-identifier
                                     point
                                     new-name)
  "Change environment name to NEW-NAME in current position.
This will change most-inner environment containing the POINT position
the file specified by TEXT-DOCUMENT-IDENTIFIER."
  (interactive
   (list
    (lsp-text-document-identifier)
    (point)
    (read-string "New environment name: ")))
  (lsp-workspace-command-execute "texlab.changeEnvironment"
                                 (vector
                                  (list :textDocument text-document-identifier
                                        :position (lsp-point-to-position point)
                                        :newName new-name))))

(declare-function graphviz-dot-mode "ext:graphviz-dot-mode")
(defun lsp-latex-show-dependency-graph ()
  "Show dependency graph written by DOT format.
`graphviz-dot-mode' is needed if you needs syntax highlights
or a graphical image."
  (interactive)
  (let ((dot-language-text (lsp-workspace-command-execute "texlab.showDependencyGraph"))
        (buffer (get-buffer-create "*lsp-latex: Dependency Graph*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert dot-language-text)
      (when (require 'graphviz-dot-mode nil t)
        (graphviz-dot-mode))
      (read-only-mode +1))
    (pop-to-buffer buffer)))

(provide 'lsp-latex)
;;; lsp-latex.el ends here
