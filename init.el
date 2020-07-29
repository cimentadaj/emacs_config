;; Use MELPA as the package repository
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa". "https://melpa.org/packages/"))

(package-initialize)

;; Use 'use-package' to download packages
;; automatically from MELPA
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/myinit.org"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-lintr-linters
   "with_defaults(trailing_whitespace_linter = NULL, commented_code_linter = NULL, camel_case_linter = NULL, snake_case_linter = NULL, object_usage_linter = NULL)")
 '(org-agenda-files
   (quote
    ("~/google_drive/gtd/gcal.org" "~/google_drive/gtd/inbox.org")))
 '(package-selected-packages
   (quote
    (helm-rg json-mode highlight-symbol highlight-numbers highlight-operators highlight-escape-sequences solaire-mode all-the-icons-ivy all-the-icons-dired centaur-tabs dashboard pyenv-mode flycheck-julia unicode-math-input org-gcal jedi company-jedi company-quickhelp flycheck free-keys expand-region easy-kill elpy doom-modeline doom-themes tao-theme poet-theme faff-theme zerodark-theme alect-themes base16-theme zenburn-theme color-theme-modern magit stan-snippets stan-mode poly-R yaml-mode helm-ag helm-projectile ag counsel-projectile projectile which-key use-package try smartparens org-ref org-bullets moe-theme ewal-spacemacs-themes ewal-spacemacs-theme ess dracula-theme counsel company auto-complete ace-window)))
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-comment-italic t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit- ace-jump-face-foreground :height 3.0))))
 '(highlight-symbol-face ((t (:background "#44444f"))))
 '(line-number ((t (:foreground "#414B4f" :background "#282B2E"))))
 '(line-number-current-line ((t (:foreground "#616B6f" :background "#282B2E")))))
(put 'downcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
