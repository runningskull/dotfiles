(use-package ag
  :commands projectile-ag
  :bind (:map evil-normal-state-map
         (",s" . projectile-ag))
  :config
  (progn
    (setq ag-highlight-search t)))


(provide 'my-mode-ag)
