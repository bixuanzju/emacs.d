;; Initialize cask to get the correct version of org-mode
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'ob-tangle)
;; (setq debug-on-error t)
(org-babel-load-file
 (expand-file-name "emacs-init.org"
                   user-emacs-directory))
