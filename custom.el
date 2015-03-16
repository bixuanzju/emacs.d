;; Define vars here
(defvar my-init-file (expand-file-name "emacs-init.el" user-emacs-directory)
  "All configurations stored in this file.")

(defvar my-org-file (expand-file-name "emacs-init.org" user-emacs-directory)
  "All configurations tangled from this file.")

(defvar org-load-path (list (concat user-emacs-directory "packages/"))
  "List of directories to search for org files to load.")

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
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight normal :height 171 :width normal))))
 '(agda2-highlight-datatype-face ((t (:foreground "azure4"))))
 '(agda2-highlight-function-face ((t (:foreground "aquamarine3"))))
 '(agda2-highlight-module-face ((t (:foreground "DarkOliveGreen2"))))
 '(agda2-highlight-primitive-type-face ((t (:foreground "SkyBlue3"))))
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "orange1"))))
 '(cursor ((t (:background "gray78" :foreground "#DCDCCC"))))
 '(flyspell-incorrect ((t (:inherit nil :underline (:color "yellow" :style wave)))))
 '(hydra-face-amaranth ((t (:foreground "dark salmon" :weight bold))))
 '(hydra-face-blue ((t (:foreground "white" :weight bold))))
 '(hydra-face-red ((t (:foreground "#758BC6" :weight bold))))
 '(idris-loaded-region-face ((t (:background "#073642"))))
 '(idris-semantic-data-face ((t (:foreground "light pink"))))
 '(idris-semantic-type-face ((t (:foreground "LightGoldenrod3"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :underline t))) t)
 '(org-block-end-line ((t (:inherit org-meta-line :underline t))) t)
 '(persp-selected-face ((t (:inherit nil :foreground "yellow"))))
 '(shm-current-face ((t (:background "#2B2B2B"))))
 '(sr-active-path-face ((t (:weight bold :height 180)))))
