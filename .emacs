(add-to-list 'load-path "~/.emacs.d/lisp/")

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
    helm ace-jump-mode
    magit ack-and-a-half
    color-theme-solarized)
  "A list of packages to ensure are installed at launch")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; enable
(require 'evil)
(require 'evil-leader)
(require 'ack-and-a-half)
(require 'magit)

;; configure
(evil-mode 1)
(evil-leader/set-leader ",")

;; other packages
(push "~/.emacs.d/lisp/helm-cmd-t" load-path)
(require 'helm-config)
(require 'helm-cmd-t)
(global-set-key (kbd "M-t") 'helm-cmd-t)

(setq helm-ff-lynx-style-map nil
      helm-input-idle-delay 0.1
      helm-idle-delay 0.1)



;; ~~~~~ CONFIGURATION ~~~~~

;; run lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; indent lines automatically
(electric-indent-mode t)

;; highlight matching parens
(show-paren-mode t)

;; don't highlight current line
(hl-line-mode nil)

;; code completion
(global-set-key (kbd "M-/") 'hippie-expand)

;; offline hyperspec
(setq common-lisp-hyperspec-root "/Users/runningskull/documentation/lisp hyperspec/HyperSpec-7-0/Hyperspec")

;; colors like whoa
(load-theme 'solarized-dark t)

;; fonts like whoa
(set-default-font "Source Code Pro")

;; evil - making the most of SPC and RET
;; (from http://emacswiki.org/emacs/Evil#toc11)
(defun my-move-key (keymap-from keymap-to key)
  "Moves keybindings from one keymap to another, deleting from the old location"
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

;; vim keymaps
(require 'my-keymaps)

;; helm
(global-set-key (kbd "C-c h") 'helm-mini)
(helm-mode 1)

;; no like idle-highlight
(remove-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

;; always enable hs-minor-mode
(add-hook 'prog-mode-hook #'hs-minor-mode)
