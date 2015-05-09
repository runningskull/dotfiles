;; ~~~~~ LOAD PATHS ~~~~~

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/emacs-color-theme-solarized")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/color-theme-base16")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/color-theme-minimal-light")

(setq exec-path (append exec-path '("/usr/local/bin")))


;; ~~~~~ EMACS STARTUP ~~~~~

(progn
  ;; Turn off mouse interface early in startup to avoid momentary display
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

;; convenience macro
(if (fboundp 'with-eval-after-load)
    (defmacro after (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(with-eval-after-load ,feature ,@body))
  (defmacro after (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


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
  '(paredit
    smex ag
    evil evil-leader evil-nerd-commenter evil-paredit
    projectile flx-ido ido-vertical-mode
    auto-complete ;ace-jump-mode 
    hideshowvis
    scss-mode js-comint
    magit
    lua-mode
    clojure-mode cider
    python web-mode)
  "A list of packages to ensure are installed at launch")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; enable
(require 'magit)
(require 'auto-complete-config)
(progn
  (dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'paredit-mode)))

(require 'projectile)
(require 'hideshowvis)
(require 'surround)
(require 'ido-vertical-mode)
(require 'flx-ido)
(require 'recentf)
(require 'web-mode)
(require 'ace-jump-mode)

(global-surround-mode 1)

;; ace-jump
(setq ace-jump-mode-scope 'window)

;; recentf
(recentf-mode 1)
(setq recentf-max-menu-items 250)
(add-to-list 'recentf-exclude "elpa")
(add-to-list 'recentf-exclude ".ido.last")
(defun ido-recentf-open ()
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; hideshowvis
(autoload 'hideshowvis-enable "hideshowvis" "enable hideshowvis")
(autoload 'hideshowvis-minor-mode "hideshowvis" "fold symbols" 'interactive)
(hideshowvis-symbols)

;; auto-complete mode
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20130330.1836/dict")
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
(setq ac-auto-start 2)

;; projectile
(setq projectile-require-project-root nil)
(projectile-global-mode)

;; ido-mode
(ido-mode t)
(ido-vertical-mode 1)
;(ido-ubiquitous t)
(flx-ido-mode 1)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 30
      ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

(defun bind-ido-keys ()
  (interactive)
  (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-k") 'ido-prev-match))

(add-hook 'ido-setup-hook 'bind-ido-keys)


;; mouse in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))


(provide 'my-init)
