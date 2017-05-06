;;;; General

(global-hl-line-mode 1)

(setq ring-bell-function 'ignore)

(show-paren-mode t)
(setq-default truncate-lines t)

;; don't ever automatically open new splits horizontally
(setq split-height-threshold 999999)


;; More prominent minibuffer

(set-face-font 'minibuffer-prompt "Hack-15") 
(set-face-attribute 'minibuffer-prompt nil :weight 'bold)



;;;; GUI-specific

(when window-system
  (use-package exec-path-from-shell
    :config (exec-path-from-shell-initialize))

  ;; default theme
  (color/light)

  (fringe-mode '(10 . 0))
  (menu-bar-mode)

  ;; osx+dired
  (setq insert-directory-program (executable-find "gls"))

  ;; default window size
  (set-frame-size (selected-frame) 96 64)

  ;; sanity
  (setq mac-command-modifier 'meta)

  ;; goodies
  (load-theme 'mnml-light t)
  (set-default-font "Source Code Pro-12")

  ;; mouse behavior
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse t)

  ;; slightly taller modeline
  (setq-default
   mode-line-format
   (cons (propertize "\u200b" 'display '((raise -0.2) (height 1.1)))
         mode-line-format))

  ;; emojis
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
  )



;;;; Terminal-specific

(unless window-system
  ;; default theme
  (color/dark)

  (menu-bar-mode 0)
  
  ;; mouse scrolling and text selection
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
  (setq mouse-wheel-progressive-speed nil)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 3)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 3)))

  (use-package pbcopy
    :config (turn-on-pbcopy))


  ;; since cmd doesn't work in terminal, we have these.
  ;; got iTerm shortcuts mapped to input that text using non-terminal shortcuts 
  ; (define-key evil-normal-state-map ",,x" 'smex) ; for the terminal
  ; (define-key evil-visual-state-map ",,x" 'smex) ; for the terminal
  ; (define-key evil-normal-state-map ",,j" 'avy-goto-line)
  ; (define-key evil-normal-state-map ",,k" 'avy-goto-line)
  )


(provide 'my-gui)
