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

(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash")
;; (bind-key "M-ƒ" 'toggle-frame-fullscreen)

;; (setq-default locate-command "mdfind")
(setq mac-right-command-modifier 'super)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; (global-set-key (kbd "M-`") 'ns-next-frame)
;; (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
;; (global-set-key (kbd "M-'") 'ns-do-hide-others)

(when (string= user-login-name "jeremybi")
  ;; (setq display-time-world-list '(("Asia/Shanghai" "Shanghai")
  ;;                                 ("America/Vancouver" "Vancouver")))
  ;; Use GNU ls - install with:
  (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")
  ;; ProofGeneral
  (load-file "/Users/jeremybi/scratch/ProofGeneral/ProofGeneral/generic/proof-site.el"))

(provide 'cocoa-emacs-default)

;;; cocoa-emacs-default.el ends here
