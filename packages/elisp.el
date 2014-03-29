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

;; (add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
;; (defun my-goto-match-beginning ()
;;   (when (and isearch-forward isearch-other-end)
;;     (goto-char isearch-other-end)))

;; (defadvice isearch-exit (after my-goto-match-beginning activate)
;;   "Go to beginning of match."
;;   (when (and isearch-forward isearch-other-end)
;;     (goto-char isearch-other-end)))

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

(defmacro my-delq-multi (list &rest elems)
  "Delete ELEMS from LIST."
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
