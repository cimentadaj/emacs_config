
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(package-selected-packages
   (quote
    (helm helm-core org-ref auto-complete moe-theme smartparens smartparents ewal-spacemacs-them ewal-spacemacs-themes dracula-theme ewal-spacemacs-theme spacemacs-theme counsel swiper ace-window org-bullets which-key try use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit- ace-jump-face-foreground :height 3.0)))))

(setq default-directory "~/")
(setq inhibit-startup-message t)
(tool-bar-mode -1)

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

;; Package to try packages for only that session
(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Start emacs with a full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable ido package to make auto-completion
;; better for completing commands in mini-buffer
(setq indo-enable-flex-matching t)
(setq indo-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-mode 1
)

;; Org-mode
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))

(defalias 'list-buffers 'ibuffer)

;;------------------------------------------------------------------------------
;; `org-ref':
;;------------------------------------------------------------------------------
(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-notes "~/Google Drive/literature/notes.org"
        org-ref-default-bibliography  '("~/Google Drive/literature/references.bib")
        org-ref-pdf-directory "~/Google Drive/literature/pdfs/"))

;; Package to place numbers on the windows to switch
;; quicker
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit- ace-jump-face-foreground :height 3.0)))))
    ))

;; Package for searching stuff through emacs (swiper)

;; counsel is used by swiper so install before
(use-package counsel
  :ensure t
  )

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    ))

;; Set spacemacs theme
;; This is a bit weird because the package is actually 'spacemacs-theme'
;; but I can't find it on MELPA through emacs (although it is on melpa.org)
;; However, this ewal-spacemacs-themes seems to work
;; (use-package ewal-spacemacs-themes
;;   :ensure t
;;   :config (load-theme 'spacemacs-dark t))

(use-package moe-theme
  :ensure t
  :config
  (setq moe-theme-highlight-buffer-id t)
  (moe-dark))

(set-face-attribute 'default nil :font "Monaco-13")


;; Install smartparens for adding parens when typing
(use-package smartparens
  :ensure t
  :config (smartparens-global-mode t))

;; Show line numbering on all buffers
;; Ideally, I'd like only line numbering
;; on the left buffers but I don't know
;; how to do it.
(global-display-line-numbers-mode 1)

;; Load ESS
(use-package ess
  :ensure t
  :init (require 'ess-site))

;; Use auto completion with auto-complete
(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

;;  (define-key ac-completing-map [tab] 'ac-complete)
;;  (define-key ac-completing-map [return] nil)

;; (setq ess-default-style 'RStudio)
;; (setq ess-use-auto-complete t)

;; Don't restore history or save on exit
(setq-default inferior-R-args "--no-restore-history --no-save")

;; Smartparens in R repl
(add-hook 'ess-R-post-run-hook (lambda () (smartparens-mode 1)))
(add-hook 'inferior-ess-mode-hook (lambda () (smartparens-mode 1)))

;; Make Shift-Enter do a lot in ESS
(setq ess-ask-for-ess-directory nil)
(add-hook 'inferior-ess-mode-hook
    '(lambda nil
          (define-key inferior-ess-mode-map [\C-up]
              'comint-previous-matching-input-from-input)
          (define-key inferior-ess-mode-map [\C-down]
              'comint-next-matching-input-from-input)
          (define-key inferior-ess-mode-map [\C-x \t]
              'comint-dynamic-complete-filename)
     )
 )

(setq ess-ask-for-ess-directory nil)
  (setq ess-local-process-name "R")
  (setq ansi-color-for-comint-mode 'filter)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)

  (defun my-ess-start-R ()
    (interactive)
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))

;; Bind M-enter to ess-eval-region-or-line-and-step
;; (define-key ess-mode-map (kbd "M-<return>") 'ess-eval-region-or-line-and-step)


;;   (defun my-ess-eval ()
;;     (interactive)
;;     (my-ess-start-R)
;;     (if (and transient-mark-mode mark-active)
;;         (call-interactively 'ess-eval-region)
;;       (call-interactively 'ess-eval-line-and-step)))
;; ;; I was trying to add that whenever it finds a paragraph it executes the complete
;; ;; parapgraph or a function. However, I couldn't get it to work and now I use
;; ;; C-c C-c which executes it as I want it.
;; ;;      (call-interactively 'ess-eval-region-or-function-or-paragraph-and-step)))
;;   (add-hook 'ess-mode-hook
;;             '(lambda()
;;                (local-set-key [(shift return)] 'my-ess-eval)))
;;   (add-hook 'inferior-ess-mode-hook
;;             '(lambda()
;;                (local-set-key [C-up] 'comint-previous-input)
;;                (local-set-key [C-down] 'comint-next-input)))
;;  (add-hook 'Rnw-mode-hook
;;           '(lambda()
;;              (local-set-key [(shift return)] 'my-ess-seval)))
;;  (require 'ess-site)


;; Add load Shiny shortcut
(defun ess-r-shiny-run-app (&optional arg)
  "Interface for `shiny::runApp()'.
   With prefix ARG ask for extra args."
  (interactive)
  (inferior-ess-r-force)
  (ess-eval-linewise
   "shiny::runApp(\".\")\n" "Running app" arg
   '("" (read-string "Arguments: " "recompile = TRUE"))))

;; avy for moving quickly through files
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char-2))

;; On startup
;; Open TODO list
(find-file "~/Google Drive/gtd/inbox.org")
