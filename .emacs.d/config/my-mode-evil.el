(defun my-send-string-to-terminal (string)
  (unless (display-graphic-p) (send-string-to-terminal string)))

(defun my-evil-term-cursor-bar ()
  (my-send-string-to-terminal "\e]50;CursorShape=1\x7"))

(defun my-evil-term-cursor-box ()
  (my-send-string-to-terminal "\e]50;CursorShape=0\x7"))

(defun my-evil-terminal-cursor-change ()
  (when (string= (getenv "TERM_PROGRAM") "iTerm.app")
    (add-hook 'evil-insert-state-entry-hook 'my-evil-term-cursor-bar)
    (add-hook 'evil-insert-state-exit-hook  'my-evil-term-cursor-box))
  (when (and (getenv "TMUX") (string= (getenv "TERM_PROGRAM") "iTerm.app"))
    (add-hook 'evil-insert-state-entry-hook (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=1\x7\e\\")))
    (add-hook 'evil-insert-state-exit-hook  (lambda () (my-send-string-to-terminal "\ePtmux;\e\e]50;CursorShape=0\x7\e\\")))))

(defun indent-buffer ()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))


(use-package evil
  :demand t
  :init
  (progn
    (setq evil-normal-state-cursor '(box))
    (setq evil-insert-state-cursor '(bar))
    (setq evil-default-cursor t)
    (evil-mode 1))
  :config
  (progn
    (add-hook 'after-make-frame-functions (lambda (frame) (my-evil-terminal-cursor-change)))

    (define-key evil-normal-state-map (kbd "M-q") 'save-buffers-kill-terminal)

    ;; Escape everything
    (define-key evil-normal-state-map [escape] (lambda () (interactive) (keyboard-quit) (my-evil-term-cursor-box)))
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

    ;; Run commands
    (define-key evil-normal-state-map (kbd "M-x") 'smex)
    (define-key evil-insert-state-map (kbd "M-x") 'smex)
    (define-key evil-visual-state-map (kbd "M-x") 'smex)
    (define-key evil-normal-state-map ",,x" 'smex) ; for the terminal
    (define-key evil-visual-state-map ",,x" 'smex) ; for the terminal

    ;; suspend
    (define-key evil-normal-state-map "Z" 'suspend-emacs)

    ;; Movement
    ;; TODO: move these to my-mode-avy.el
    (define-key evil-normal-state-map (kbd "M-j") 'avy-goto-line)
    (define-key evil-normal-state-map (kbd "M-k") 'avy-goto-line)
    (define-key evil-normal-state-map (kbd ",,j") 'avy-goto-line)
    (define-key evil-normal-state-map (kbd ",,k") 'avy-goto-line)

    ;; Folding
    (define-key evil-normal-state-map ", " 'evil-toggle-fold)

    ;; Comments
    (define-key evil-normal-state-map "gc" 'comment-dwim-line)

    ;; tpope
    (evil-define-key 'normal dired-mode-map "-" 'my-dired-vinegar) 

    ;; NeoTree
    (add-hook
     'neotree-mode-hook
     (lambda ()
       (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
       (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
       (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
       (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
       (define-key evil-normal-state-local-map (kbd "-") 'neotree-select-up-node)
       (define-key evil-normal-state-local-map (kbd "o") 'neotree-change-root)
       ))

    ;; Jump to Buffers
    (define-key evil-normal-state-map ",mm" 'ido-switch-buffer)
    (define-key evil-normal-state-map ",mf" 'ibuffer)
    (define-key evil-normal-state-map ",ms" (lambda () (interactive) (switch-to-buffer "*scratch*")))
    (define-key evil-normal-state-map ",mM" (lambda () (interactive) (switch-to-buffer "*Messages*")))
    (define-key evil-normal-state-map (kbd "C-i") 'ido-switch-buffer)

    ;; Jump to files
    (define-key evil-normal-state-map ",or" 'ido-recentf-open)
    (define-key evil-normal-state-map (kbd "C-\\") 'ido-recentf-open)
    (define-key evil-normal-state-map (kbd "M-t") 'projectile-find-file)
    (define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)

    ;; jump to most recent buffer
    (defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer))))
    (define-key evil-normal-state-map ",b," 'switch-to-previous-buffer)
    (define-key evil-normal-state-map ",z" 'switch-to-previous-buffer)

    ;; Split windows
    (define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-insert-state-map (kbd "M-h") 'evil-window-left)
    (define-key evil-insert-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-insert-state-map (kbd "M-l") 'evil-window-right)
    (define-key evil-insert-state-map (kbd "C-l") 'evil-window-right)
    (define-key evil-insert-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-insert-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "M-|") 'evil-window-vsplit)
    (define-key evil-normal-state-map (kbd "M-_") 'evil-window-split)

    ;; Elisp eval
    (define-key evil-normal-state-map ",ee" 'eval-last-sexp)
    (define-key evil-normal-state-map ",e," 'eval-defun)

    ;; Help
    (define-key evil-normal-state-map ",hk" 'describe-key)
    (define-key evil-normal-state-map ",hf" 'describe-function)
    (define-key evil-normal-state-map ",hv" 'describe-variable)

    ;; Search
    (define-key evil-normal-state-map ",s" 'projectile-ag)

    ;; More accurate marks by "default"
    (define-key evil-motion-state-map "'" 'evil-goto-mark)
    (define-key evil-motion-state-map "`" 'evil-goto-mark-line)

    ;; Navigate errors
    (define-key evil-normal-state-map ",er" 'next-error)
    (define-key evil-normal-state-map ",ep" 'previous-error)

    ;; Magit
    ;; TODO: move these to my-mode-magit.el

    ;; Paredit
    ;; TODO: move these to my-mode-paredit.el
    (define-key evil-normal-state-map ",(" 'paredit-wrap-round)
    (define-key evil-normal-state-map ",)" 'paredit-wrap-round)
    (define-key evil-normal-state-map ",[" 'paredit-wrap-square)
    (define-key evil-normal-state-map ",]" 'paredit-wrap-square)
    (define-key evil-normal-state-map ",{" 'paredit-wrap-curly)
    (define-key evil-normal-state-map ",}" 'paredit-wrap-curly)
    (define-key evil-visual-state-map ",(" 'paredit-wrap-round)
    (define-key evil-visual-state-map ",)" 'paredit-wrap-round)
    (define-key evil-visual-state-map ",[" 'paredit-wrap-square)
    (define-key evil-visual-state-map ",]" 'paredit-wrap-square)
    (define-key evil-visual-state-map ",{" 'paredit-wrap-curly)
    (define-key evil-visual-state-map ",}" 'paredit-wrap-curly)
    (define-key evil-normal-state-map ",,(" 'paredit-convert-to-round)
    (define-key evil-normal-state-map ",,[" 'paredit-convert-to-square)
    (define-key evil-normal-state-map ",,{" 'paredit-convert-to-curly)
    (define-key evil-normal-state-map ",,<" 'paredit-convert-to-angled)
    (define-key evil-normal-state-map "<" 'paredit-backward)
    (define-key evil-normal-state-map ">" 'paredit-forward)
    (define-key evil-normal-state-map (kbd "M->") 'paredit-forward-slurp-sexp)
    (define-key evil-normal-state-map (kbd "M-<") 'paredit-forward-barf-sexp)
    (define-key evil-normal-state-map (kbd "C-<") 'paredit-backward-slurp-sexp)
    (define-key evil-normal-state-map (kbd "C->") 'paredit-backward-barf-sexp)
    (define-key evil-insert-state-map (kbd "M->") 'paredit-forward-slurp-sexp)
    (define-key evil-insert-state-map (kbd "M-<") 'paredit-forward-barf-sexp)
    (define-key evil-insert-state-map (kbd "C->") 'paredit-backward-slurp-sexp)
    (define-key evil-insert-state-map (kbd "C-<") 'paredit-backward-barf-sexp)
    ;; combined w/ iterm mapping, allows use of meta in terminal
    (define-key evil-normal-state-map (kbd ",,>") 'paredit-forward-slurp-sexp)
    (define-key evil-normal-state-map (kbd ",,<") 'paredit-forward-barf-sexp)

    ;; misc

    ;; maximize
    (define-key evil-normal-state-map (kbd ",w-") (lambda () (interactive) (toggle-frame-fullscreen)))

    ;; sometimes it doesn't enable properly, so use this to enable
    (define-key evil-normal-state-map (kbd ",fs") 'hideshowvis-enable)
    
    ;; mice are for humans
    (define-key evil-normal-state-map [left-fringe mouse-1] 'hideshowvis-click-fringe)

    ;; Expand-region
    (define-key evil-normal-state-map "#" 'er/expand-region)

    ;; reindent whole buffer
    (define-key evil-normal-state-map (kbd ",, TAB") 'indent-buffer)


    (unless window-system
      ;; quick window split
      (define-key evil-normal-state-map (kbd "C-_") 'evil-window-vsplit)
      ;; Bar/block cursors in terminal
      (my-evil-terminal-cursor-change))))


(-whichkey-section- "Folding" ",f")
(-whichkey-section- "Buffers" ",b")
(-whichkey-section- "Windows" ",w")
(-whichkey-section- "Elisp" ",e")
(-whichkey-section- "Help" ",h")


(provide 'my-mode-evil)

