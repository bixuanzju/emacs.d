;;; better-defaults.el --- Better Emacs Default Settings -*- lexical-binding: t -*-

;; Copyright (C) Jeremy Bi

;; Author: Jeremy Bi <bixuanzju@qq.com>
;; Maintainer: Jeremy Bi <bixuanzju@qq.com>
;; Created:  13 Mar 2014
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
;; (if (fboundp 'fringe-mode)
;;     (fringe-mode 4))

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

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)
(setq abbrev-file-name (expand-file-name "abbrev_defs" savefile-dir))

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
;; (defun prelude-auto-save-command ()
;;   "Save the current buffer if `prelude-auto-save' is not nil."
;;   (when (and buffer-file-name
;;              (buffer-modified-p (current-buffer))
;;              (file-writable-p buffer-file-name))
;;     (save-buffer)))

;; (defmacro advise-commands (advice-name commands class &rest body)
;;   "Apply advice named ADVICE-NAME to multiple COMMANDS.
;; The body of the advice is in BODY."
;;   `(progn
;;      ,@(mapcar (lambda (command)
;;                  `(defadvice ,command (,class ,(intern (concat (symbol-name command) "-" advice-name)) activate)
;;                     ,@body))
;;                commands)))

;; advise all window switching functions
;; (advise-commands "auto-save"
;;                  (ace-window
;;                   switch-to-buffer
;;                   other-window)
;;                  before
;;                  (prelude-auto-save-command))

;; (add-hook 'mouse-leave-buffer-hook 'prelude-auto-save-command)

(defadvice kill-region (before smart-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; automatically indenting yanked text if in programming-modes
(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) prelude-yank-indent-threshold)
      (indent-region beg end nil)))

;; (advise-commands "indent" (yank yank-pop) after
;;   "If current mode is one of `prelude-yank-indent-modes',
;; indent yanked text (with prefix arg don't indent)."
;;   (if (and (not (ad-get-arg 0))
;;            (not (member major-mode prelude-indent-sensitive-modes))
;;            (or (derived-mode-p 'prog-mode)
;;                (member major-mode prelude-yank-indent-modes)))
;;       (let ((transient-mark-mode nil))
;;         (yank-advised-indent-function (region-beginning) (region-end)))))

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (with-current-buffer buffer (if mode (funcall mode)))))

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

;; clean up obsolete buffers automatically
(require 'midnight)

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

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

(defun prelude-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

;; Newline at end of file
(setq require-final-newline t)

(defun prelude-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (eval-after-load "subword"
    '(diminish 'subword-mode))
  (eval-after-load "eldoc"
    '(diminish 'eldoc-mode))
  (prelude-local-comment-auto-fill)
  (prelude-font-lock-comment-annotations))

(add-hook 'prog-mode-hook 'prelude-prog-mode-defaults)

(provide 'better-defaults)

;;; better-defaults.el ends here
