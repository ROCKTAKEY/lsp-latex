;;; lsp-latex.el --- lsp-mode client for LaTeX, on texlab     -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: languages, tex

;; Version: 1.0.6

;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0"))
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
;;; lsp-mode client for LaTeX.
;;; How to Use?
;;   - First, you have to install ~texlab~.
;;     Please install this from https://github.com/latex-lsp/texlab/releases .
;;   - Next, you should make ~lsp-mode~ available.
;;     See https://github.com/emacs-lsp/lsp-mode.
;;   - Now, you can use Language Server Protocol (LSP) on (la)tex-mode or
;;     yatex-mode just to evaluate this:

;;   (add-to-list 'load-path "/path/to/lsp-latex")
;;   (require 'lsp-latex)
;;   ;; "texlab" must be located at a directory contained in `exec-path'.
;;   ;; If you want to put "texlab" somewhere else,
;;   ;; you can specify the path to "texlab" as follows:
;;   ;; (setq lsp-latex-texlab-executable "/path/to/texlab")

;;   (with-eval-after-load "tex-mode"
;;    (add-hook 'tex-mode-hook 'lsp)
;;    (add-hook 'latex-mode-hook 'lsp))

;;   ;; For YaTeX
;;   (with-eval-after-load "yatex"
;;    (add-hook 'yatex-mode-hook 'lsp))

;;; Note
;;   In this package, you can use even texlab v0.4.2 or older, written with Java,
;;   though it is not recommended.

;;; License
;;   This package is licensed by GPLv3. See the file "LICENSE".

;;; Code:
(require 'lsp-mode)
(require 'cl-lib)

(defgroup lsp-latex nil
  "Language Server Protocol client for LaTeX."
  :group 'lsp-mode)


;;; For texlab v0.4.2 or older.
(defcustom lsp-latex-java-executable "java"
  "Executable command to run Java.
This is used with `lsp-latex-java-argument-list'.

This variable is only for texlab v0.4.2 or older.  If you use newer,
You don't have to set or care about this variable."
  :group 'lsp-latex
  :type 'string)

(defcustom lsp-latex-java-argument-list '("-jar")
  "List of arguments passed to `lsp-latex-java-executable'.

This variable is only for texlab v0.4.2 or older.  If you use newer,
You don't have to set or care about this variable."
  :group 'lsp-latex
  :risky t
  :type '(repeat string))

(defcustom lsp-latex-texlab-jar-file 'search-from-exec-path
  "File named \"texlab.jar\".
You can install it from https://github.com/latex-lsp/texlab/releases/ .

The value can be a string (path to \"texlab.jar\") or the symbol
search-from-exec-path. See the docstring of `lsp-latex-get-texlab-jar-file'.

This variable is only for texlab v0.4.2 or older. If you use newer,
You don't have to set or care about this variable."
  :group 'lsp-latex
  :type '(choice string (const search-from-exec-path)))

(defcustom lsp-latex-texlab-jar-argument-list '()
  "List of arguments passed to `lsp-latex-texlab-jar-file'.

This variable is only for texlab v0.4.2 or older.  If you use newer,
You don't have to set or care about this variable."
  :group 'lsp-latex
  :type '(repeat string))

(defun lsp-latex-get-texlab-jar-file ()
  "Return the path to \"texlab.jar\".

If `lsp-latex-texlab-jar-file' is a string, return it.
If `lsp-latex-texlab-jar-file' is the symbol search-from-exec-path,
then search a file named \"texlab.jar\" from `exec-path'.

This function is only for texlab v0.4.2 or older. If you use newer,
You don't have to set or care about this variable."
  (cond
   ((stringp lsp-latex-texlab-jar-file)
    lsp-latex-texlab-jar-file)
   ((eq lsp-latex-texlab-jar-file 'search-from-exec-path)
    (locate-file "texlab.jar" exec-path))
   (t (error "Invalid value of `lsp-latex-texlab-jar-file'"))))


;;; For texlab v1.0.0 or newer.
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



(defcustom lsp-latex-forward-search-executable nil
  "Executable command used to search in preview.
It is passed server as \"latex.forwardSearch.executable\"."
  :group 'lsp-latex
  :type 'string)

(defcustom lsp-latex-forward-search-args nil
  "List of arguments passed with `lsp-latex-forward-search-executable.'
 It is passed server as \"latex.forwardSearch.executable\"."
  :group 'lsp-latex
  :type '(repeat string))

(add-to-list 'lsp-language-id-configuration '(".*\\.tex$" . "latex"))

(defun lsp-latex-new-connection ()
  "Create new connection of lsp-latex."
  (let (jar-file)
    (cond
     ((locate-file lsp-latex-texlab-executable exec-path)
      (cons lsp-latex-texlab-executable
            lsp-latex-texlab-executable-argument-list))
     ((setq jar-file (lsp-latex-get-texlab-jar-file))
      (append
       (cons
        lsp-latex-java-executable
        lsp-latex-java-argument-list)
       (cons
        jar-file
        lsp-latex-texlab-jar-argument-list)))
     (t
      (error "No executable \"texlab\" file")))))

;; Copied from `lsp-clients--rust-window-progress' in `lsp-rust'.
(defun lsp-latex-window-progress (_workspace params)
  "Progress report handling.
PARAMS progress report notification data."
  ;; Minimal implementation - we could show the progress as well.
  (lsp-log (gethash "title" params)))

(lsp-register-client
 (setq
  lsp-latex--client
  (make-lsp-client :new-connection
                   (lsp-stdio-connection
                    #'lsp-latex-new-connection)
                   :major-modes '(tex-mode yatex-mode latex-mode)
                   :server-id 'texlab
                   :initialized-fn
                   (lambda (workspace)
                     (with-lsp-workspace workspace
                       (lsp--set-configuration
                        `(:latex
                          (:forwardSearch
                           (,@(when lsp-latex-forward-search-executable
                                `(:executable
                                  ,lsp-latex-forward-search-executable))
                            ,@(when lsp-latex-forward-search-args
                                `(:args ,lsp-latex-forward-search-args))))))))
                   :notification-handlers
                   (lsp-ht
                    ("window/progress"
                     'lsp-latex-window-progress)))))



(defun lsp-latex--message-result-only-fail (result)
  "Message unless RESULT means success."
  (message
   (cl-case (plist-get result :status)
     ((1)                             ;Error
      "Build do not succeeded.")
     ((2)                             ;Failure
      "Build failed.")
     ((3)                             ;Cancelled
      "Build cancelled."))))

(defun lsp-latex-forward-search ()
  "Forward search on preview."
  (interactive)
  (lsp-request-async
   "textDocument/forwardSearch"
   (lsp--text-document-position-params)
   #'lsp-latex--message-result-only-fail))

(provide 'lsp-latex)
;;; lsp-latex.el ends here
