;; EasyMotion
(define-key evil-normal-state-map ",,w" 'ace-jump-mode)
(define-key evil-normal-state-map ",,j" 'ace-jump-line-mode)
(define-key evil-normal-state-map ",,k" 'ace-jump-line-mode)
(define-key evil-normal-state-map ",,c" 'ace-jump-char-mode)

;; Folding
(define-key evil-normal-state-map ", " 'evil-toggle-fold)
(define-key evil-normal-state-map ",ff" 'evil-open-folds)
(define-key evil-normal-state-map ",fd" 'evil-close-folds)

;; Split windows
(define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)

;; execute things
(define-key evil-normal-state-map ",xx" 'smex)
(define-key evil-normal-state-map ",xe" 'eval-last-sexp)

;; swap to most recent buffer
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))
(define-key evil-normal-state-map ",z" 'switch-to-previous-buffer)


(provide 'my-keymaps)
