;;; elisp.el --- Some helper functions -*- lexical-binding: t -*-

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

(use-package dash-functional
  :ensure dash-functional
  :config
  (dash-enable-font-lock))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(defun toggle-window-split ()
  "Toggle window splitting between horizontal to vertical."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
;; (defun my-goto-match-beginning ()
;;   (when (and isearch-forward isearch-other-end)
;;     (goto-char isearch-other-end)))

;; (defadvice isearch-exit (after my-goto-match-beginning activate)
;;   "Go to beginning of match."
;;   (when (and isearch-forward isearch-other-end)
;;     (goto-char isearch-other-end)))

(defun backward-kill-word-or-kill-region ()
  "Kill region if there's one, otherwise kill the a word backward."
  (interactive)
  (call-interactively (if (region-active-p)
                          'kill-region
                        'backward-kill-word)))

;; after deleting a tag, indent properly
;; (defadvice sgml-delete-tag (after reindent activate)
;;   (indent-region (point-min) (point-max)))

;; (defmacro after-load (feature &rest body)
;;   "After FEATURE is loaded, evaluate BODY."
;;   (declare (indent defun))
;;   `(eval-after-load ,feature
;;      '(progn ,@body)))

;; (defmacro rename-modeline (package-name mode new-name)
;;   "Rename PACKAGE-NAME MODE to NEW-NAME."
;;   `(eval-after-load ,package-name
;;      '(defadvice ,mode (after rename-modeline activate)
;;         (setq mode-name ,new-name))))

;; (rename-modeline "js2-mode" js2-mode "JS2")
;; (rename-modeline "clojure-mode" clojure-mode "Clj")
;; (rename-modeline "haskell-mode" haskell-mode "HS")

;; (defun isml ()
;;   "If sml repl exists, then restart it else create a new repl."
;;   (interactive)
;;   (when (get-buffer "*sml*")
;;     (with-current-buffer "*sml*"
;;       (when (process-live-p "sml")
;;         (comint-send-eof)))
;;     (sleep-for 0.2))
;;   (sml-run "sml" ""))

;; (defadvice sml-prog-proc-load-file
;;   (before fresh-sml-repl-then-load-file activate)
;;   "Create a new repl before reload the file."
;;   (isml))

;; (defadvice delete-frame
;;   (around avoid-accidently-delete-frame activate)
;;   (when (yes-or-no-p "Quit Emacs? ")
;;     ad-do-it))

;; toggel shell escape using C-c C-t C-x
(defun TeX-toggle-escape ()
  "Toggle Shell Escape."
  (interactive)
  (setq LaTeX-command
        (if (string= LaTeX-command "latex") "latex -shell-escape"
          "latex"))
  (message (concat "shell escape "
                   (if (string= LaTeX-command "latex -shell-escape")
                       "enabled"
                     "disabled"))))
(add-hook 'LaTeX-mode-hook
          (lambda nil
            (local-set-key (kbd "C-c C-t x") 'TeX-toggle-escape)))

(defun eshell/clear ()
  "Clears the shell buffer ala Unix's clear."
  ;; the shell prompts are read-only, so clear that for the duration
  (let ((inhibit-read-only t))
    ;; simply delete the region
    (delete-region (point-min) (point-max))))

;; (defun my-find-user-custom-file ()
;;   "Edit the `custom-file', in another window."
;;   (interactive)
;;   (find-file custom-file))

;; (defun my-recompile-personal-file ()
;;   "Byte-compile all your dotfiles again."
;;   (interactive)
;;   (byte-recompile-directory prelude-vendor-dir 0))

(defmacro delq-multi (list &rest elems)
  "Delete members of LIST, which are in ELEMS."
  (let ((result (mapcar (lambda (elem)
                          `(setq ,list (delq ,elem ,list)))
                        elems)))
    `(progn ,@result)))

(defun prelude-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun prelude-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun prelude-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(provide 'elisp)

;;; elisp.el ends here
