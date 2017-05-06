(-whichkey-section- "Magit" ",g")


;;;; Helpers fns

(defun magit-fringe-click (evt)
  (interactive "e")
  (mouse-set-point evt)
  (magit-section-toggle (magit-current-section)))



;;;; Init

(use-package magit
  :defer t
  :diminish magit-auto-revert-mode
  :bind (:map magit-mode-map
         ("M-j" . magit-section-forward)
         ("M-k" . magit-section-backward)
         (":" . evil-ex)
         (";" . magit-git-command)
         ([left-fringe mouse-1] . magit-fringe-click)
         :map evil-normal-state-map
         (",gg" . magit-status)
         (",gb" . magit-blame)
         (",gB" . magit-blame-quit)
         (",gl" . magit-log-popup)
         ))



(provide 'my-mode-magit)
