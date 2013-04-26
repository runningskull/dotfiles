;; EasyMotion
(define-key evil-normal-state-map ",,w" 'ace-jump-mode)
(define-key evil-normal-state-map ",,j" 'ace-jump-line-mode)
(define-key evil-normal-state-map ",,k" 'ace-jump-line-mode)
(define-key evil-normal-state-map ",,c" 'ace-jump-char-mode)

;; Folding
(define-key evil-normal-state-map ", " 'evil-toggle-fold)
(define-key evil-normal-state-map ",ff" 'evil-open-folds)
(define-key evil-normal-state-map ",fd" 'evil-close-folds)
(define-key evil-normal-state-map ",fs0" 'hs-hide-all)
(define-key evil-normal-state-map ",fs9" 'hs-show-all)

;; Open buffer
(define-key evil-normal-state-map ",b" 'ido-switch-buffer)

;; Quit
(define-key evil-normal-state-map (kbd "M-q") 'save-buffers-kill-terminal)

;; Split windows
(define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)

;; execute things
(define-key evil-normal-state-map ",xx" 'smex)
(define-key evil-normal-state-map ",xe" 'eval-last-sexp)

(add-hook 'js3-mode-hook
          (lambda ()
            (define-key evil-normal-state-map ",x " 'js-send-region-and-go)))
                                          

;; insert a tab
(define-key evil-insert-state-map (kbd "M-<tab>") 'quoted-insert)

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
(define-key evil-normal-state-map ",or" 'helm-recentf)
(define-key evil-normal-state-map ",op" 'helm-projectile)
(define-key evil-normal-state-map (kbd "M-t") 'helm-projectile)


;; ack
(define-key evil-normal-state-map ",s" 'ack-and-a-half)


(provide 'my-keymaps)
