;; ~~~~~ LOAD PATHS ~~~~~

(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/emacs-color-theme-solarized")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/color-theme-base16")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/color-theme-mnml-light")

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
    auto-complete avy ;ace-jump-mode 
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
(require 'hideshow)
(require 'hideshowvis)
(require 'ido-vertical-mode)
(require 'flx-ido)
(require 'recentf)
;; (require 'web-mode)
(require 'expand-region)
(require 'evil-surround) ; cloned repo
;;(require 'ace-jump-mode)

;; my plugins
(require 'swap-windows)



;;;; Color Themes

;; (load-theme 'solarized-dark t)
;; (load-theme 'base16-ocean-dark t)
(load-theme 'mnml-light t)
(set-default-font "Source Code Pro-12")


;;;; Mode Settings

(global-evil-surround-mode 1)

;; ace-jump
                                        ;(setq ace-jump-mode-scope 'window)

;; avy mode (ace-jump replacement)
(setq avy-keys (number-sequence ?a ?z))
(setq avy-all-windows nil)

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


;; gnu-go
(defun enable-gnugo-image-display ()
  (interactive)
  (require 'gnugo-xpms))


;; expand-region
(defun limited-expand-region ()
  (setq er/try-expand-list '(er/mark-inside-pairs
                             er/mark-outside-pairs
                             er/mark-defun)))

(add-hook 'paredit-mode-hook 'limited-expand-region)


;; spaceline
(setq ns-use-srgb-colorspace nil)
(setq powerline-default-separator 'slant) 
(setq spaceline-separator-dir-left '(right . right))
(setq spaceline-separator-dir-right '(left . left))
(setq spaceline-minor-modes-separator " ")
(require 'powerline)
(require 'powerline-evil)
(require 'spaceline-config)
(spaceline-emacs-theme)

(eval-after-load "vc-hooks"
  '(defadvice vc-mode-line (after sml/after-vc-mode-line-advice () activate)
     "Color `vc-mode'."
     (when (stringp vc-mode)
       (let ((noback (concat "↣" (replace-regexp-in-string (format "^ %s[:-]" (vc-backend buffer-file-name)) "" vc-mode))))
         (setq vc-mode
               (propertize noback
                           'face (cond ((string-match "^ -" noback)    'sml/vc)
                                       ((string-match "^ [:@]" noback) 'sml/vc-edited)
                                       ((string-match "^ [!\\?]" noback) 'sml/modified))))))))



;; diminish
(require 'diminish) 
(eval-after-load "paredit" '(diminish 'paredit-mode)) 
(eval-after-load "undo-tree" '(diminish 'undo-tree-mode)) 
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode)) 
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode)) 
(eval-after-load "aggressive-indent" '(diminish 'aggressive-indent-mode))
(eval-after-load 'hideshow '(diminish 'hs-minor-mode)) 
(eval-after-load 'eldoc '(diminish 'eldoc-mode))
(setq projectile-mode-line '(:eval (format " «%s»" (projectile-project-name)))) 
(setq spaceline-highlight-face #'spaceline-highlight-face-evil-state)
(spaceline-toggle-buffer-encoding-abbrev-off)



;; Mouse in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))



(defun spaceline-jr-theme (&rest additional-segments)
  "Install a modeline close to the one used by Spacemacs, but which
looks better without third-party dependencies.
ADDITIONAL-SEGMENTS are inserted on the right, between `global' and
`buffer-position'."
  (spaceline-install

   '(((((workspace-number window-number) :separator "|")
       buffer-modified
       buffer-size)
      :face highlight-face)
     anzu
     ((buffer-id remote-host)
      :face highlight-face)
     major-mode
     ((flycheck-error flycheck-warning flycheck-info)
      :when active)
     (((minor-modes :separator spaceline-minor-modes-separator)
       process)
      :when active)
     (erc-track :when active)
     (version-control :when active)
     (org-pomodoro :when active)
     (org-clock :when active)
     nyan-cat)

   `((battery :when active)
     selection-info
     ((buffer-encoding-abbrev
       point-position
       line-column)
      :separator " | ")
     (global :when active)
     ,@additional-segments
     buffer-position
     hud)))


(defun dim-inactive-window ()
  (interactive)
  (font-lock-mode))

(defun brighten-active-window ()
  (interactive)
  (font-lock-mode))

(require 'auto-dim-other-buffers)


(provide 'my-init)
