;; Load customization
;; Keep emacs custom-settings in separate file
(defvar my-custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file my-custom-file)

;; (add-to-list 'load-path my-vendor-dir)

;; (setq use-package-verbose t)

(if (file-exists-p my-init-file)
    (load-file my-init-file)
  (progn
    (org-babel-load-file
     (expand-file-name "emacs-init.org" user-emacs-directory))))
