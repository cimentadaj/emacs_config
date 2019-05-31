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
