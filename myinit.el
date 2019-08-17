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

;; Set lines to continue if they're too long instead of
;; continuing them in the next line
(setq-default truncate-lines t)
;; (setq toggle-truncate-lines t)
;; (setq truncate-partial-width-windows nil)

;; ;; On startup open TODO list
(find-file "~/google_drive/gtd/inbox.org")

;; Remaps kill this buffer to familiar CTRL + W
(global-set-key [(control w)] 'kill-this-buffer)
(global-set-key (kbd "M-k")  'kill-region)

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

(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))

(add-hook 'prog-mode-hook #'whitespace-mode)

(setq-default fill-column 80)

(use-package org-bullets
	:ensure t
	:config
	(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1))))
;;        (add-hook 'text-mode-hook 'flyspell-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))

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

;; (use-package moe-theme
;;   :ensure t
;;   :config
;;   (setq moe-theme-highlight-buffer-id t)
;;   (moe-dark))

;; (set-face-attribute 'default nil :font "Monaco-13")

(use-package doom-themes
  :ensure t)



;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

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

(use-package poly-R
  :ensure t)

(use-package poly-markdown
  :ensure t)

;;; R modes
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.rmd" . poly-markdown+r-mode))

;; Export files with the same name as the main file
;; Taken from https://github.com/karawoo/prelude/blob/db60a8e448757b1e07b7323e411c3d5d4d1b7d45/personal/custom.el#L751-L752
(setq polymode-exporter-output-file-format "%s")

;; Don't restore history or save on exit
(setq-default inferior-R-args "--no-restore-history --no-save")

(setq ess-ask-for-ess-directory nil)

;; ESS doesn't slow down Emacs
;; (setq ess-eval-visibly 'nowait) ;; in 12.09-1
(setq ess-eval-visibly nil)
;; Smartparens in R repl.
(add-hook 'ess-R-post-run-hook (lambda () (smartparens-mode 1)))
(add-hook 'inferior-ess-mode-hook (lambda () (smartparens-mode 1)))

;; Set the style to RStudio. This gives me stuff like tab spaces are 2 spaces not 4
(setq ess-default-style 'RStudio)

;; Doesn't work until now. I think the variable display-buffer-alist
  (setq display-buffer-alist
	'(("*R*"
	   nil
	   (dedicated . t))))

; Set up company, i.e. code autocomplete
  (use-package company
    :ensure t
    :config
    ;; Enable company mode everywhere
    (add-hook 'after-init-hook 'global-company-mode))

  ;; Set up TAB to manually trigger autocomplete menu
  (define-key company-mode-map (kbd "TAB") 'company-complete)
  (define-key company-active-map (kbd "TAB") 'company-complete-common)
  ;; Set up M-h to see the documentation for items on the autocomplete menu
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)

  ;; If you would want to have the help pop ups in company mode
  ;; (use-package company-quickhelp
  ;;   :ensure t
  ;;   :config
  ;;   (company-quickhelp-mode))

  ;;   (use-package auto-complete
  ;;     :ensure t
  ;;     :init
  ;;     (progn
  ;;       (ac-config-default)
  ;;       (global-auto-complete-mode t)
  ;;       ))

  ;;   ;; To allow for TAB completion
  ;;   ;; https://stackoverflow.com/questions/49232454/emacs-ess-how-to-auto-complete-library-function
  ;;   (use-package company
  ;;     :ensure t
  ;;     :init (require 'company))

  ;;   (setq tab-always-indent 'complete)

  ;;   (setq company-idle-delay 0.5
  ;; 	company-show-numbers t
  ;; 	company-minimum-prefix-length 2
  ;; 	company-tooltip-flip-when-above t)

  ;;   (global-set-key (kbd "C-M-/") #'company-complete)
  ;;   (global-company-mode)
  ;;   (defun my-ess-hook ()
  ;;     ;; ensure company-R-library is in ESS backends
  ;;     (make-local-variable 'company-backends)
  ;;     (cl-delete-if (lambda (x) (and (eq (car-safe x) 'company-R-args))) company-backends)
  ;;     (push (list 'company-R-args 'company-R-objects 'company-R-library :separate)
  ;; 	  company-backends))
  ;; 	  (add-hook 'ess-mode-hook 'my-ess-hook)
  ;; 	  (with-eval-after-load 'ess
  ;;     (setq ess-use-company t))

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

  (defun my-ess--R ()
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

(defun new-chunk (header)
  "Insert an r-chunk in markdown mode. Necessary due to interactions between polymode and yasnippet"
  (interactive "sHeader: ")
  (insert (concat "```{r " header "}\n\n```"))
  (forward-line -1))

(use-package stan-mode
     :ensure t
     :init (require 'stan-mode))

   (use-package stan-snippets
     :ensure t
     :init (require 'stan-snippets))

(setq stan-use-auto-complete t)

(use-package magit
      :ensure t
      :init
      (progn
      (bind-key "C-x g" 'magit-status)
      ))

(define-key magit-mode-map (kbd "<tab>") 'magit-section-toggle)


  ;; (setq magit-status-margin
  ;;   '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))
  ;;     (use-package git-gutter
  ;;     :ensure t
  ;;     :init
  ;;     (global-git-gutter-mode +1))

  ;;     (global-set-key (kbd "M-g M-g") 'hydra-git-gutter/body)


  ;;     (use-package git-timemachine
  ;;     :ensure t
  ;;     )
  ;;   (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
  ;; 			      :hint nil)
  ;;     "
  ;;   Git gutter:
  ;;     _j_: next hunk        _s_tage hunk     _q_uit
  ;;     _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ;;     ^ ^                   _p_opup hunk
  ;;     _h_: first hunk
  ;;     _l_: last hunk        set start _R_evision
  ;;   "
  ;;     ("j" git-gutter:next-hunk)
  ;;     ("k" git-gutter:previous-hunk)
  ;;     ("h" (progn (goto-char (point-min))
  ;; 		(git-gutter:next-hunk 1)))
  ;;     ("l" (progn (goto-char (point-min))
  ;; 		(git-gutter:previous-hunk 1)))
  ;;     ("s" git-gutter:stage-hunk)
  ;;     ("r" git-gutter:revert-hunk)
  ;;     ("p" git-gutter:popup-hunk)
  ;;     ("R" git-gutter:set-start-revision)
  ;;     ("q" nil :color blue)
  ;;     ("Q" (progn (git-gutter-mode -1)
  ;; 		;; git-gutter-fringe doesn't seem to
  ;; 		;; clear the markup right away
  ;; 		(sit-for 0.1)
  ;; 		(git-gutter:clear))
  ;; 	 :color blue))

(setq python-shell-interpreter "python3")

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(defun assign_python_operator ()
  "Python - Insert = operator"
  (interactive)
  (insert " = "))
(define-key python-mode-map (kbd "C-<") 'assign_python_operator)
(define-key inferior-python-mode-map (kbd "C-<") 'assign_python_operator)

(defun python-scratch ()
  (interactive)
  (progn
    (delete-other-windows)
    (setq new-buf (get-buffer-create "scratch.py"))
    (switch-to-buffer new-buf)
    (python-mode)
    (setq w1 (selected-window))
    (setq w1name (buffer-name))
    (setq w2 (split-window w1 nil t))
    (if (not (member "*Python*" (mapcar (function buffer-name) (buffer-list))))
	(run-python))
    (set-window-buffer w2 "*Python*")
    (set-window-buffer w1 w1name)))

(global-set-key (kbd "C-x 7") 'python-scratch)

(use-package yasnippet
  :ensure t
  :init
    (yas-global-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

'(flycheck-lintr-caching nil) ;; need to customised it inside of Emacs
(add-hook 'ess-mode-hook
	  (lambda () (flycheck-mode t)))

'(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
'(flycheck-idle-change-delay 4) ;; Set delay based on what suits you the best

;; In case you want to add flycheck every time you save.
;; (setq flycheck-check-syntax-automatically '(save mode-enable))
;; the default value was '(save idle-change new-line mode-enabled)
