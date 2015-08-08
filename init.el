;; (package-initialize)

;; Define vars here
(defvar my-init-file (expand-file-name "emacs-init.el" user-emacs-directory)
  "All configurations stored in this file.")

(defvar my-org-file (expand-file-name "emacs-init.org" user-emacs-directory)
  "All configurations tangled from this file.")

(defvar my-vendor-dir (expand-file-name "packages/" user-emacs-directory)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")

(defvar savefile-dir (expand-file-name "savefile/" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")

(defvar prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar prelude-yank-indent-modes '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar prelude-yank-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfs--current-profile-name "profile1" t)
 '(cfs--fontsize-steps (quote (7 4 4)) t)
 '(cfs--profiles-english-fontsizes (quote (18 12.5 12.5)) t)
 '(cfs--profiles-fontsizes (quote (18 12.5 12.5)) t)
 '(column-number-mode t)
 '(coq-prog-args (quote ("-I" "/Users/jeremybi/Dropbox/cpdt/src")))
 '(history-length 1000)
 '(indicate-empty-lines t)
 '(load-prefer-newer t)
 '(max-lisp-eval-depth 2000)
 '(package-selected-packages
   (quote
    (helm-projectile projectile osx-trash crosshairs magit zop-to-char zenburn-theme worf whitespace-cleanup-mode wgrep-ag vlf visual-regexp-steroids use-package undo-tree tuareg sml-mode smex smartparens smart-mode-line-powerline-theme skeletor reveal-in-finder rainbow-mode racket-mode quickrun popwin pandoc-mode pallet ox-pandoc ov omnisharp mwim multiple-cursors multi-term move-text markdown-mode lua-mode lispy lexbind-mode key-chord js2-mode idris-mode hindent helm-descbinds helm-ag gscholar-bibtex goto-chg git-timemachine ggtags fullframe flycheck-haskell fix-word eyebrowse expand-region exec-path-from-shell ensime elpy ebib easy-kill dired+ dash-at-point company-ghci company-coq clojure-mode chinese-fonts-setup cdlatex bison-mode auctex-latexmk anzu)))
 '(set-mark-command-repeat-pop t)
 '(shift-select-mode nil)
 '(split-height-threshold nil)
 '(split-width-threshold 130)
 '(user-mail-address "bixuanzju@qq.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-datatype-face ((t (:foreground "azure4"))))
 '(agda2-highlight-function-face ((t (:foreground "aquamarine3"))))
 '(agda2-highlight-module-face ((t (:foreground "DarkOliveGreen2"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "SkyBlue3"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "orange1"))))
 '(flyspell-incorrect ((t (:inherit nil :underline (:color "yellow" :style wave)))))
 '(idris-loaded-region-face ((t (:background "#073642"))) t)
 '(idris-semantic-data-face ((t (:foreground "light pink"))))
 '(idris-semantic-type-face ((t (:foreground "LightGoldenrod3"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :underline t))) t)
 '(org-block-end-line ((t (:inherit org-meta-line :underline t))) t)
 '(persp-selected-face ((t (:inherit nil :foreground "yellow"))))
 '(shm-current-face ((t (:background "#2B2B2B"))))
 '(sr-active-path-face ((t (:weight bold :height 180)))))

;; (add-to-list 'load-path my-vendor-dir)

(if (file-exists-p my-init-file)
    (load-file my-init-file)
  (progn
    (org-babel-load-file
     (expand-file-name "emacs-init.org" user-emacs-directory))))
