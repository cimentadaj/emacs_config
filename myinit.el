;; Sets default directory
(setq default-directory "~/")
;; Inhibits the Emacs startup message w/ tutorial, etc..
(setq inhibit-startup-message t)
(tool-bar-mode -1)

;; Whenever emacs asks for yes or no, just y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Reload buffer without reopening with F5
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Start emacs with full screen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Show line numbering on all buffers
;; Ideally, I'd like only line numbering
;; on the left buffers but I don't know
;; how to do it.
(global-display-line-numbers-mode)

;; To highlight complementary parenthesis when cursor is on top
(show-paren-mode 1)
(setq show-paren-delay 0)

(setq org-src-tab-acts-natively t)

;; On startup open TODO list
(find-file "~/google_drive/gtd/inbox.org")

;; store all backup and autosave files in the tmp dir
  (setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))

(use-package try
  :ensure t)

(setq indo-enable-flex-matching t)
(setq indo-everywhere t)
(setq ido-create-new-buffer 'always)
(ido-mode 1)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package smartparens
  :ensure t
  :config (smartparens-global-mode t))

(use-package org-bullets
	:ensure t
	:config
	(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))
;;        (add-hook 'text-mode-hook 'flyspell-mode))



(defalias 'list-buffers 'ibuffer)

(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit- ace-jump-face-foreground :height 3.0)))))
    ))

(use-package org-ref
  :ensure t
  :config
  (setq reftex-default-bibliography '("~/google_drive/literature/references.bib")
	org-ref-bibliography-notes "~/google_drive/literature/notes.org"
	org-ref-default-bibliography  '("~/google_drive/literature/references.bib")
	org-ref-pdf-directory "~/google_drive/literature/pdfs/"))

(use-package yaml-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

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

;; avy for moving quickly through files
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char-2))

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

;; If you find an error, ag needs to be installed from terminal as well.
;; homebrew install the_silver_searcher for macs
;; sudo apt-get install silversearcher-ag from ubuntu

  ;; Helm search for projectile. Allows to search for files within a project
  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on))
  
  ;; Needed by helm-projectile for esearch
  (use-package helm-ag
    :ensure t)

  (use-package projectile
    :ensure t
    :bind ("C-c p" . projectile-command-map)
    :config
    (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile))


    ;; (use-package counsel-projectile
    ;; :ensure t
    ;; :config
    ;; ;; (counsel-projectile-mode))

(use-package ess
  :ensure t
  :init (require 'ess-site))

;; Don't restore history or save on exit
(setq-default inferior-R-args "--no-restore-history --no-save")

;; Smartparens in R repl.
(add-hook 'ess-R-post-run-hook (lambda () (smartparens-mode 1)))
(add-hook 'inferior-ess-mode-hook (lambda () (smartparens-mode 1)))

;; Set the style to RStudio. This gives me stuff like tab spaces are 2 spaces not 4
(setq ess-default-style 'RStudio)

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)
    ))

;; To allow for TAB completion
;; https://stackoverflow.com/questions/49232454/emacs-ess-how-to-auto-complete-library-function
(use-package company
  :ensure t
  :init (require 'company))

(setq tab-always-indent 'complete)

(setq company-idle-delay 0.5
      company-show-numbers t
      company-minimum-prefix-length 2
      company-tooltip-flip-when-above t)

(global-set-key (kbd "C-M-/") #'company-complete)
(global-company-mode)
(defun my-ess-hook ()
  ;; ensure company-R-library is in ESS backends
  (make-local-variable 'company-backends)
  (cl-delete-if (lambda (x) (and (eq (car-safe x) 'company-R-args))) company-backends)
  (push (list 'company-R-args 'company-R-objects 'company-R-library :separate)
	company-backends))
	(add-hook 'ess-mode-hook 'my-ess-hook)
	(with-eval-after-load 'ess
  (setq ess-use-company t))

;; Taken from https://github.com/karawoo/prelude/blob/db60a8e448757b1e07b7323e411c3d5d4d1b7d45/personal/custom.el
;; %>% shortcut
;; http://emacs.stackexchange.com/a/8055/7060
(defun then_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (insert " %>% "))
(define-key ess-mode-map (kbd "C->") 'then_R_operator)
(define-key inferior-ess-mode-map (kbd "C->") 'then_R_operator)

(defun assign_R_operator ()
  "R - Insert <- operator"
  (interactive)
  (insert " <- "))
(define-key ess-mode-map (kbd "C-<") 'assign_R_operator)
(define-key inferior-ess-mode-map (kbd "C-<") 'assign_R_operator)

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

(defun R-scratch ()
  (interactive)
  (progn
    (delete-other-windows)
    (setq new-buf (get-buffer-create "scratch.R"))
    (switch-to-buffer new-buf)
    (R-mode)
    (setq w1 (selected-window))
    (setq w1name (buffer-name))
    (setq w2 (split-window w1 nil t))
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
	(R))
    (set-window-buffer w2 "*R*")
    (set-window-buffer w1 w1name)))

(global-set-key (kbd "C-x 9") 'R-scratch)

(defun ess-r-shiny-run-app (&optional arg)
  "Interface for `shiny::runApp()'.
   With prefix ARG ask for extra args."
  (interactive)
  (inferior-ess-r-force)
  (ess-eval-linewise
   "shiny::runApp(\".\")\n" "Running app" arg
   '("" (read-string "Arguments: " "recompile = TRUE"))))
