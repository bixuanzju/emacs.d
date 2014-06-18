;; Initialize cask to get the correct version of org-mode
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; Load customization
(defvar my-init-file (expand-file-name "emacs-init.el" user-emacs-directory)
  "All configurations stored in this file.")

(if (file-exists-p my-init-file)
    (load-file my-init-file)
  (progn
    (org-babel-load-file
     (expand-file-name "emacs-init.org" user-emacs-directory))))
