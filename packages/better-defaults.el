;;; better-defaults.el --- Better Emacs Default Settings -*- lexical-binding: t -*-

;; Copyright (C) Jeremy Bi

;; Author: Jeremy Bi <bixuanzju@qq.com>
;; Maintainer: Jeremy Bi <bixuanzju@qq.com>
;; Created:  13 Mar 2014
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

;;------[Begin Sanity]--------------------------------------------------

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " Jeremy - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Smooth Scroll:
(setq mouse-wheel-scroll-amount '(1 ((shift) .1))) ;; one line at a time

(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Remove alarm (bell) on scroll
(setq ring-bell-function 'ignore)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; disable annoying blink-matching-paren
(setq blink-matching-paren nil)

;; Autosave buffers when focus is lost
(defun prelude-save-all-buffers ()
  "Save all modified buffers, without prompts."
  (save-some-buffers 'dont-ask))

(when (version<= "24.4" emacs-version)
  (add-hook 'focus-out-hook 'prelude-save-all-buffers))

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; highlight the current line
(global-hl-line-mode +1)

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; enable winner-mode to manage window configurations
(winner-mode +1)

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" savefile-dir))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; (require 'desktop)
;; (setq desktop-save t)
;; (setq desktop-path (list savefile-dir))
;; (setq desktop-dirname savefile-dir)
;; (desktop-save-mode +1)

(defun prelude-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

(defun prelude-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (eval-after-load "subword"
    '(diminish 'subword-mode))
  (smartparens-mode 1)
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (prelude-smart-open-line-above)) "RET")))
  (prelude-local-comment-auto-fill)
  (prelude-font-lock-comment-annotations))

(add-hook 'prog-mode-hook 'prelude-prog-mode-defaults)

;; Newline at end of file
(setq require-final-newline t)

;; Ido-related config
(require 'ido)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-save-directory-list-file (expand-file-name "ido.hist" savefile-dir)
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)
(ido-mode +1)

;; disable ido faces to see flx highlights
(setq ido-use-faces nil)

(req-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode +1))

(req-package flx-ido
  :config
  (flx-ido-mode +1))

(req-package ido-vertical-mode
  :config
  (ido-vertical-mode +1))

(provide 'better-defaults)

;;; better-defaults.el ends here
