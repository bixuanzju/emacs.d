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
  :config
  (dash-enable-font-lock))

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

(defun prelude-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun org-require (orgfile)
  "orgfile is a symbol to be loaded"
  (let ((org-file (concat (symbol-name orgfile) ".org"))
        (path))

    ;; find the org-file
    (catch 'result
      (loop for dir in org-load-path do
            (when (file-exists-p
                   (setq path
                         (concat
                          (directory-file-name dir)
                          "/"
                          org-file)))
              (throw 'result path))))
    (org-babel-load-file path)))

;; (defun my/record-mark ()
;;   (deactivate-mark)
;;   (ring-insert find-tag-marker-ring (point-marker)))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line
  number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing command.
         ;; Remove this first conditional if you don't want it.
         (cond ((ignore-errors (org-edit-src-code))
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

;; (define-key endless/toggle-map "n" #'narrow-or-widen-dwim)

(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun duplicate-comment-current-line-or-region (arg)
  "Duplicates and/or comments the current line. If there's no
region, the current line will be duplicated.  However, if there's
a region, all lines that region covers will be duplicated."
  (interactive "P")
  (pcase-let* ((origin (- (point) (line-beginning-position)))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (if arg
        (comment-or-uncomment-region beg end))
    (setq end (line-end-position))
    (goto-char end)
    (newline)
    (insert region)
    (goto-char (+ (line-beginning-position) origin))))

(defun prelude-cleanup-buffer-or-region ()
  "Cleanup a region if selected, otherwise the whole buffer."
  (interactive)
  (call-interactively 'untabify)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (call-interactively 'indent-region))
  (whitespace-cleanup))

(defun prelude-kill-other-buffers ()
  "Kill all buffers but the current one.
  Doesn't mess with special buffers."
  (interactive)
  (-each
      (->> (buffer-list)
        (-filter #'buffer-file-name)
        (--remove (eql (current-buffer) it)))
    #'kill-buffer)
  (message "Done"))

(defun my/forward-line-by-many ()
  "Move line forward by multiple times."
  (interactive)
  (forward-line 4))

(defun my/backward-line-by-many ()
  "Move line backward by multiple times."
  (interactive)
  (forward-line -4))

(provide 'elisp)


;;; elisp.el ends here
