(define-key evil-normal-state-map [escape] 'keyboard-quit)
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

;; EasyMotion
(define-key evil-normal-state-map ",,w" 'ace-jump-mode)
(define-key evil-normal-state-map ",,j" 'ace-jump-line-mode)
(define-key evil-normal-state-map ",,k" 'ace-jump-line-mode)
(define-key evil-normal-state-map ",,c" 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "M-j") 'ace-jump-line-mode)
(define-key evil-normal-state-map (kbd ",,j") 'ace-jump-line-mode)
(define-key evil-normal-state-map (kbd "M-k") 'ace-jump-line-mode)
(define-key evil-normal-state-map (kbd ",,k") 'ace-jump-line-mode)

;; Folding
(define-key evil-normal-state-map ", " 'evil-toggle-fold)
(define-key evil-normal-state-map ",ff" 'evil-open-folds)
(define-key evil-normal-state-map ",fd" 'evil-close-folds)
(define-key evil-normal-state-map ",fs0" 'hs-hide-all)
(define-key evil-normal-state-map ",fs12" 'hs-show-all)
(define-key evil-normal-state-map ",fsl" 'hs-hide-level)

;; Commenting
(define-key evil-normal-state-map "gc" 'paredit-comment-dwim)

;; Open buffer
(define-key evil-normal-state-map ",b" 'ido-switch-buffer)

;; Quit
(define-key evil-normal-state-map (kbd "M-q") 'save-buffers-kill-terminal)

;; Suspend
(define-key evil-normal-state-map "Z" 'suspend-emacs)

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

;; execute emacs lisp
(define-key evil-normal-state-map ",,x" 'smex)
(define-key evil-normal-state-map ",ee" 'eval-last-sexp)
(define-key evil-normal-state-map ",ed" 'eval-defun)

;; barebones autocomplete
(define-key evil-insert-state-map (kbd "C-\\") 'hippie-expand)


;; commenting
(define-key evil-normal-state-map ",c " 'evilnc-comment-or-uncomment-lines)

;; help
(define-key evil-normal-state-map ",hk" 'describe-key)
(define-key evil-normal-state-map ",hf" 'describe-function)

(add-hook 'js3-mode-hook
          (lambda ()
            (define-key evil-normal-state-map ",x " 'js-send-region-and-go)))
                                          

;; dammit, insert the character I tell you too!
(define-key evil-insert-state-map (kbd "M-j") 'quoted-insert)

;; JS comma first
(defun js-leading-comma ()
  (interactive)
  (insert "  , "))
(define-key evil-insert-state-map (kbd "M-,") 'js-leading-comma)

;; swap to most recent buffer
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))
(define-key evil-normal-state-map ",z" 'switch-to-previous-buffer)


;; open recent file
(define-key evil-normal-state-map ",or" 'ido-recentf-open)
(define-key evil-normal-state-map (kbd "M-t") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)


;; silver searcher
(define-key evil-normal-state-map ",s" 'projectile-ag)


;; more accurate marks by "default"
(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)

;; indentation
(define-key evil-visual-state-map ",g" 'indent-region)
(define-key evil-normal-state-map ",g" 'indent-sexp)


(provide 'my-keymaps)
