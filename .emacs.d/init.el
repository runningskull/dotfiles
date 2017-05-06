;; Bootstrap
(add-to-list 'load-path "~/.emacs.d/config")
(package-initialize)
(prefer-coding-system 'utf-8)


;; -- Core -- ;;

;; Emacs startup tweaks
(require 'my-startup)

;; Boilerplate utils
(require 'my-elisp-utils)
(require 'my-package-init)

;; Emacs core setup
(require 'my-loadpaths)
(require 'my-theme)
(require 'my-customizations)
(require 'my-backups)
(require 'my-history)
(require 'my-gui)



;; -- Third-party Packages -- ;;

;; Bwahahahaha
(require 'my-mode-evil)

;; If package requires any additional config,
;; move into its own my-mode-*.el file
(use-package smex :defer t)



;; -- Language Configurations -- ;;

(require 'my-lang-clojure)



;; -- Mode Configurations -- ;;

;; Built-in
(require 'my-mode-dired)
(require 'my-mode-recentf)
(require 'my-mode-paredit)
(require 'my-mode-undo-tree)

;; 3rd Party
(require 'my-mode-ag)
(require 'my-mode-avy)
(require 'my-mode-cider)
(require 'my-mode-company)
(require 'my-mode-hideshow)
(require 'my-mode-ido)
(require 'my-mode-magit)
(require 'my-mode-projectile)
(require 'my-mode-which-key)
