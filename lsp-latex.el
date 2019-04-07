;;; lsp-latex.el --- lsp-mode client for LaTeX.      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: languages, extensions, tex

;; Version: 0.0.0

;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0"))

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

;;

;;; Code:
(require 'lsp-mode)
(require 'seq)

(defgroup lsp-latex nil
  "Language Server Protocol client for LaTeX."
  :group 'lsp-mode)

(defcustom lsp-latex-java-executable "java"
  "Executable command to run Java.
This is used with `lsp-latex-java-argument-list'."
  :group 'lsp-latex
  :type 'string)

(defcustom lsp-latex-java-argument-list '("-jar")
  "List of arguments passed to `lsp-latex-java-executable'."
  :group 'lsp-latex
  :risky t
  :type '(repeat string))

(defcustom lsp-latex-texlab-jar-file 'search-from-exec-path
  "File named \"texlab.jar\".
You can install it from https://github.com/latex-lsp/texlab/releases/tag/v0.4.1 .

The value can be a string (path to \"texlab.jar\") or the symbol search-from-exec-path. See the docstring of `lsp-latex-get-texlab-jar-file'."
  :group 'lsp-latex
  :type '(choice string (const search-from-exec-path)))

(defcustom lsp-latex-texlab-jar-argument-list '()
  "List of arguments passed to `lsp-latex-texlab-jar-file'. "
  :group 'lsp-latex
  :type '(repeat string))




(defun lsp-latex-get-texlab-jar-file ()
  "Return the path to \"texlab.jar\".

If `lsp-latex-texlab-jar-file' is a string, return it.
If `lsp-latex-texlab-jar-file' is the symbol search-from-exec-path, then search a file named \"texlab.jar\" from `exec-path'."
  (cond
   ((stringp lsp-latex-texlab-jar-file)
    lsp-latex-texlab-jar-file)
   ((eq lsp-latex-texlab-jar-file 'search-from-exec-path)
    (let* ((jar-filename "texlab.jar")
           (jar-dir (or (seq-find (lambda (dir)
                                    (let ((path (format "%s/%s"
                                                        dir jar-filename)))
                                      (when (file-exists-p path)
                                        path)))
                                  exec-path)
                        (error (format "\"%s\" not found in `exec-path'"
                                       jar-filename)))))
      (format "%s/%s"
              jar-dir jar-filename)))
   (t (error "invalid value of `lsp-latex-texlab-jar-file'"))))

(defun lsp-latex-new-connection ()
  ""
  (append
   (cons
    lsp-latex-java-executable
    lsp-latex-java-argument-list)
   (cons
    (lsp-latex-get-texlab-jar-file)
    lsp-latex-texlab-jar-argument-list)))

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
                    :major-modes '(tex-mode yatex-mode latex-mode)
                    :server-id 'texlab
                    :notification-handlers
                    (lsp-ht
                     ("window/progress"
                      'lsp-latex-window-progress))))

(provide 'lsp-latex)
;;; lsp-latex.el ends here
