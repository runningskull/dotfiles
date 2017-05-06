(use-package projectile
  :defer t
  :bind (:map evil-normal-state-map
         ("C-p" . projectile-find-file))
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-mode-line '(:eval (format " «%s»" (projectile-project-name)))) ))



(provide 'my-mode-projectile)
