;;; cocoa-emacs-default.el --- Config for OSX -*- lexical-binding: t -*-

;; Copyright (C) Jeremy Bi

;; Author: Jeremy Bi <xbi@zju.edu.cn>
;; Maintainer: Jeremy Bi <xbi@zju.edu.cn>
;; Created:  2 Jan 2014
;; Keywords: convenience editing
;; URL: https://github.com/bixuanzju/emacs_repo

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

(req-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")
(bind-key "M-ƒ" 'toggle-frame-fullscreen)

(setq-default locate-command "mdfind")
(setq ns-pop-up-frames nil ; Don't pop up new frames from the workspace
      mac-option-modifier 'meta
      mac-command-modifier 'meta
      mac-function-modifier 'hyper
      mac-right-option-modifier 'none
      mac-right-command-modifier 'super)

(when (string= user-login-name "jeremybi")
  (add-to-list 'default-frame-alist
               '(font . "DejaVu Sans Mono-17"))
  (setq display-time-world-list '(("Asia/Shanghai" "Shanghai")
                                ("America/Vancouver" "Vancouver")))
  ;; Use GNU ls - install with:
  (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")
  ;; ProofGeneral
  (load-file "/usr/local/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
  ;; Paradox
  (if (f-exists? "~/.paradox.el")
      (load-file "~/.paradox.el")
    (message "Remember installing Paradox?")))

(provide 'cocoa-emacs-default)

;;; cocoa-emacs-default.el ends here
