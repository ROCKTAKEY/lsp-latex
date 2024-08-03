;;; lsp-latex-test.el --- test for lsp-latex.        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  ROCKTAKEY

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

(provide 'lsp-latex-test)
;;; lsp-latex-test.el ends here
