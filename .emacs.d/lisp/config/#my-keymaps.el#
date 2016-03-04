
;;;; Escape everything

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)



;;;; Run commands

(define-key evil-normal-state-map (kbd "M-x") 'smex)
(define-key evil-insert-state-map (kbd "M-x") 'smex)
(define-key evil-visual-state-map (kbd "M-x") 'smex)
(define-key evil-normal-state-map ",,x" 'smex)


;; Movement

(define-key evil-normal-state-map (kbd "M-j") 'avy-goto-line)
(define-key evil-normal-state-map (kbd "M-k") 'avy-goto-line)
(define-key evil-motion-state-map (kbd "M-j") #'avy-goto-line) 
(define-key evil-motion-state-map (kbd "M-k") #'avy-goto-line) 
;; for terminal
(define-key evil-normal-state-map (kbd ",,j") 'avy-goto-line)
(define-key evil-normal-state-map (kbd ",,k") 'avy-goto-line)
;;(define-key evil-motion-state-map (kbd ",,j") 'avy-goto-line)
;;(define-key evil-motion-state-map (kbd ",,k") 'avy-goto-line)



;;;; Folding

(define-key evil-normal-state-map ", " 'evil-toggle-fold)
(define-key evil-normal-state-map ",ff" 'evil-open-folds)
(define-key evil-normal-state-map ",fd" 'evil-close-folds)
(define-key evil-normal-state-map ",fs0" 'hs-hide-all)
(define-key evil-normal-state-map ",fs12" 'hs-show-all)
(define-key evil-normal-state-map ",fsl" 'hs-hide-level)



;;;; File Browsing

(defun my-dired-vinegar ()
  (interactive)
  (if (eq major-mode 'dired-mode)
      (find-alternate-file "..")
    (if (fboundp 'dired-jump)
        (dired-jump)
      (dired "."))))
(define-key evil-normal-state-map "-" 'my-dired-vinegar) 
(evil-define-key 'normal dired-mode-map "-" 'my-dired-vinegar) 



;;;; Commenting

(define-key evil-normal-state-map "gc" 'paredit-comment-dwim)



;;;; Jump to buffers

(define-key evil-normal-state-map ",bb" 'ido-switch-buffer)
(define-key evil-normal-state-map ",bs" (lambda () (interactive) (switch-to-buffer "*scratch*")))
(define-key evil-normal-state-map ",brr" (lambda () (interactive) (switch-to-buffer "*cider-repl clj.repl*")))
(define-key evil-normal-state-map ",brf" (lambda () (interactive) (switch-to-buffer "*cider-repl figwheel.repl*")))

;; jump to most recent buffer
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))
(define-key evil-normal-state-map ",z" 'switch-to-previous-buffer)



;;;; Quit

(define-key evil-normal-state-map (kbd "M-q") 'save-buffers-kill-terminal)
(define-key evil-normal-state-map "Z" 'suspend-emacs)



;;;; Split windows

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

(define-key evil-normal-state-map (kbd ",wy") 'swap-buffers-in-windows) 
(define-key evil-normal-state-map (kbd ",wp") 'swap-buffers-in-windows) 



;;;; Eval elisp

(define-key evil-normal-state-map ",ee" 'eval-last-sexp)
(define-key evil-normal-state-map ",ed" 'eval-defun)



;;;; Barebones autocomplete

(define-key evil-insert-state-map (kbd "C-\\") 'hippie-expand)



;; Commenting

(define-key evil-normal-state-map ",c " 'evilnc-comment-or-uncomment-lines)



;;;; Browse help
(define-key evil-normal-state-map ",hk" 'describe-key)
(define-key evil-normal-state-map ",hf" 'describe-function)
(define-key evil-normal-state-map ",hv" 'describe-variable)



;;;; Javascript helpers

(add-hook
 'js3-mode-hook
 (lambda ()
   (define-key evil-normal-state-map ",x " 'js-send-region-and-go)))

;; dammit, insert the character I tell you too!
(define-key evil-insert-state-map (kbd "M-j") 'quoted-insert)

;; comma first
(defun js-leading-comma ()
  (interactive)
  (insert "  , "))
(define-key evil-insert-state-map (kbd "M-,") 'js-leading-comma)


;;;; Open recent file

(define-key evil-normal-state-map ",or" 'ido-recentf-open)
(define-key evil-normal-state-map (kbd "C-\\") 'ido-recentf-open)
(define-key evil-normal-state-map (kbd "M-t") 'projectile-find-file)
(define-key evil-normal-state-map (kbd "C-p") 'projectile-find-file)



;;;; Searching

(define-key evil-normal-state-map ",s" 'projectile-ag)



;;;; More accurate marks by "default"

(define-key evil-motion-state-map "'" 'evil-goto-mark)
(define-key evil-motion-state-map "`" 'evil-goto-mark-line)


;;;; Navigate errors

(define-key evil-normal-state-map ",er" 'next-error)
(define-key evil-normal-state-map ",ep" 'previous-error)


;;;; Expand-region

(define-key evil-normal-state-map "#" 'er/expand-region)



(provide 'my-keymaps)
