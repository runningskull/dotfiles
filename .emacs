;; ~~~~~ LOAD PATHS ~~~~~

(setq load-path (append load-path '("~/.emacs.d/lisp/"
                                    "~/.emacs.d/lisp/my-config/"
                                    "~/.emacs.d/lisp/heml-cmd-t/")))

(setq custom-theme-load-path (append custom-theme-load-path
                                     '("~/.emacs.d/lisp/emacs-color-theme-solarized")))

(setq exec-path (append exec-path '("/usr/local/bin")))



;; ~~~~~ PACKAGES ~~~~~

;; initialize
(require 'package)
(setq package-archives '(("melpa". "http://melpa.milkbox.net/packages/")
                         ("ELPA" . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; ensure these are installed
(defvar my-packages
  '(starter-kit
    starter-kit-lisp starter-kit-eshell starter-kit-js
    evil evil-leader evil-nerd-commenter evil-paredit
    helm projectile helm-projectile
    auto-complete ace-jump-mode hideshowvis
    js2-mode ac-js2
    magit ack-and-a-half)
  "A list of packages to ensure are installed at launch")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; enable
(require 'magit)
(require 'auto-complete-config)
(require 'projectile)
(require 'hideshowvis)

;; non-elpa packages
;(push "~/.emacs.d/lisp/helm-cmd-t" load-path)
;(require 'helm-config)
;(require 'helm-cmd-t)
;(global-set-key (kbd "M-t") 'helm-cmd-t)
;
;(setq helm-ff-lynx-style-map nil helm-input-idle-delay 0.1 helm-idle-delay 0.1)



;; ~~~~~ CONFIGURATION ~~~~~

;; run lisp

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; indent lines automatically
(electric-indent-mode t)

;; highlight matching parens
(show-paren-mode t)

;; command key
(setq mac-command-modifier 'meta)

;; code completion
(global-set-key (kbd "M-/") 'hippie-expand)

;; offline hyperspec
(setq common-lisp-hyperspec-root "/Users/runningskull/documentation/lisp hyperspec/HyperSpec-7-0/Hyperspec")

;; colors like whoa
(load-theme 'solarized-dark t)

;; fonts like whoa
(set-default-font "Source Code Pro")

;; helm
(global-set-key (kbd "C-c h") 'helm-mini)
(helm-mode 1)

;; no like idle-highlight
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

;; hooks
(defun my-code-mode ()
  (imenu-add-menubar-index) ;; parse for folds
  (hs-minor-mode t)
  (projectile-on)
  (auto-complete-mode))
(add-hook 'prog-mode-hook (lambda ()
                            (my-code-mode)
                            (evil-close-folds)))


;; auto-complete mode
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20130330.1836/dict")
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
(setq ac-auto-start 2)


;; hideshowvis
(autoload 'hideshowvis-enable "hideshowvis" "enable hideshowvis")
(autoload 'hideshowvis-minor-mode "hideshowvis" "fold symbols" 'interactive)
(hideshowvis-symbols)


;; ~~~~~ FURTHER CONFIG ~~~~~

;; languages
(require 'my-javascript)

;; modes
(require 'my-evil)
(require 'my-keymaps)



;; ~~~~~ EMACS CUSTOMIZE ~~~~~

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-selection ((t (:background "ForestGreen" :foreground "White" :underline "White"))))
 '(helm-source-header ((t (:background "#22083397778B" :foreground "white" :weight bold :height 1.3))))
 '(hs-face ((t (:foreground "#073642" :box (:line-width 1 :color "#073642"))))))
