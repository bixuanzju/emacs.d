;;; cocoa-emacs-default.el --- Config for OSX -*- lexical-binding: t -*-

;; Copyright (C) Jeremy Bi

;; Author: Jeremy Bi <bixuanzju@qq.com>
;; Maintainer: Jeremy Bi <bixuanzju@qq.com>
;; Created:  2 Jan 2014
;; Keywords: convenience editing
;; URL: https://github.com/bixuanzju/emacs.d

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(setq mac-right-command-modifier 'super)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; (global-set-key (kbd "M-`") 'ns-next-frame)
;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
;; (global-set-key (kbd "M-'") 'ns-do-hide-others)

(when (string= user-login-name "jeremybi")
  ;; Prefer g-prefixed coreutils version of standard utilities when available
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))


  (mac-auto-operator-composition-mode 1)

  ;; copy from http://endlessparentheses.com/proof-general-configuration-for-the-coq-software-foundations-tutorial.html
  ;; ProofGeneral
  (load-file "/Users/jeremybi/Projects/ProofGeneral/generic/proof-site.el")

  ;; I appreciate the effort of writing a splash-screen, but the angry
  ;; general on the gif scares me.
  (setq proof-splash-seen t)

  ;; Hybrid mode is by far the best.
  (setq proof-three-window-mode-policy 'smart)

  ;; I don't know who wants to evaluate comments
  ;; one-by-one, but I don't.
  (setq proof-script-fly-past-comments t)

  (with-eval-after-load 'coq
    ;; The most common command by far. Having a 3(!)
    ;; keys long sequence for this command is just a
    ;; crime.
    (define-key coq-mode-map "\M-n"
      #'proof-assert-next-command-interactive)

    ;; Small convenience for commonly written commands.
    (define-key coq-mode-map "\C-c\C-m" "\nend\t")
    (define-key coq-mode-map "\C-c\C-e"
      #'endless/qed)
    (defun endless/qed ()
      (interactive)
      (unless (memq (char-before) '(?\s ?\n ?\r))
        (insert " "))
      (insert "Qed.")
      (proof-assert-next-command-interactive))

    ;; (add-hook 'coq-mode-hook 'abbrev-mode)

    ;; (define-abbrev coq-mode-abbrev-table "re" "reflexivity.")
    ;; (define-abbrev coq-mode-abbrev-table "ind" "induction")
    ;; (define-abbrev coq-mode-abbrev-table "si" "simpl.")
    ;; (advice-add 'proof-assert-next-command-interactive
    ;;             :before #'expand-abbrev)
    )

  (require 'ottmode))

(provide 'cocoa-emacs-default)

;;; cocoa-emacs-default.el ends here
