;;; lsp-latex-test.el --- test for lsp-latex.        -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2026  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords:

;; Version: 0.0.0

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

(require 'lsp-latex)
(require 'ert)
(require 'json)

(ert-deftest lsp-latex--vectorize-recursive-simple ()
  (should (equal (lsp-latex--vectorize-recursive '())
                 []))
  (should (equal (lsp-latex--vectorize-recursive '(a b c))
                 [a b c])))

(ert-deftest lsp-latex--vectorize-recursive-recursive ()
  (should (equal (lsp-latex--vectorize-recursive '(a (b) (c)))
                 [a [b] [c]]))
  (should (equal (lsp-latex--vectorize-recursive '((a b) c))
                 [[a b] c]))
  (should (equal (lsp-latex--vectorize-recursive '((a (b d (e f g))) c))
                 [[a [b d [e f g]]] c])))

(defun lsp-latex-test--environment-response ()
  "Return a Texlab response for the equation environment in the test buffer."
  (json-parse-string
   "[{\"name\": {\"text\": \"equation\",
               \"range\": {\"start\": {\"line\": 1, \"character\": 7},
                         \"end\": {\"line\": 1, \"character\": 15}}},
      \"fullRange\": {\"start\": {\"line\": 1, \"character\": 0},
                    \"end\": {\"line\": 3, \"character\": 14}}}]"
   :array-type 'list))

(ert-deftest lsp-latex-goto-environment-interactively ()
  (save-window-excursion
    (with-temp-buffer
      (setq buffer-file-name "/tmp/lsp-latex-environment.tex")
      (insert "before\n\\begin{equation}\nx\n\\end{equation}\n")
      (goto-char (point-min))
      (forward-line 2)
      (let ((buffer (current-buffer))
            (requested nil))
        (cl-letf (((symbol-function 'lsp-workspace-command-execute)
                   (lambda (command arguments)
                     (setq requested t)
                     (should (equal command "texlab.findEnvironments"))
                     (should (eq (current-buffer) buffer))
                     (should
                      (equal arguments
                             (vector
                              (list :textDocument (lsp-text-document-identifier)
                                    :position '(:line 2 :character 0)))))
                     (lsp-latex-test--environment-response)))
                  ((symbol-function 'consult--read)
                   (lambda (candidates &rest options)
                     (should (equal (plist-get options :prompt)
                                    "Goto environment: "))
                     (should (= (length candidates) 1))
                     (let ((selected
                            (funcall (plist-get options :lookup)
                                     (car candidates) candidates)))
                       (should (eq (marker-buffer selected) buffer))
                       (should (= (marker-position selected) 15))
                       (funcall (plist-get options :state) 'return selected)
                       selected))))
          (switch-to-buffer buffer)
          (call-interactively #'lsp-latex-goto-environment)
          (should requested)
          (should (eq (current-buffer) buffer))
          (should (= (point) 8)))))))

(ert-deftest lsp-latex-complete-environment-in-another-buffer ()
  (with-temp-buffer
    (setq buffer-file-name "/tmp/lsp-latex-environment.tex")
    (insert "before\n\\begin{equation}\nx\n\\end{equation}\n")
    (goto-char (point-min))
    (forward-line 2)
    (let* ((buffer (current-buffer))
           (origin (point))
           (document (lsp-text-document-identifier))
           (requested nil))
      (with-temp-buffer
        (insert (make-string origin ?x))
        (let ((caller (current-buffer)))
          (cl-letf (((symbol-function 'lsp-workspace-command-execute)
                     (lambda (command arguments)
                       (setq requested t)
                       (should (equal command "texlab.findEnvironments"))
                       (should
                        (equal arguments
                               (vector
                                (list :textDocument document
                                      :position '(:line 2 :character 0)))))
                       (should (eq (current-buffer) buffer))
                       (lsp-latex-test--environment-response)))
                    ((symbol-function 'lsp-latex--consult-mark-return)
                     (lambda (prompt markers)
                       (should (equal prompt "Choose environment: "))
                       (should (eq (marker-buffer (car markers)) buffer))
                       (should (= (marker-position (car markers)) 15))
                       (copy-marker (car markers)))))
            (let ((environment
                   (lsp-latex-complete-environment
                    buffer origin "Choose environment: ")))
              (should requested)
              (should (equal (lsp-latex-environment-location-name environment)
                             "equation"))
              (should (equal (lsp-latex-environment-location-name-region environment)
                             '(15 . 23)))
              (should (equal (lsp-latex-environment-location-full-region environment)
                             '(8 . 41)))
              (should (eq (current-buffer) caller))
              (should (= (point) (1+ origin)))
              (should (= (with-current-buffer buffer (point)) origin)))))))))

(provide 'lsp-latex-test)
;;; lsp-latex-test.el ends here
