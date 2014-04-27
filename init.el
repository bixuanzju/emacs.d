;; Initialize cask to get the correct version of org-mode
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'ob-tangle)
(org-babel-load-file
 (expand-file-name "emacs-init.org" user-emacs-directory))
