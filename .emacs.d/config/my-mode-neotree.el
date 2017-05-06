
(use-package neotree
  :defer t
  :bind (:map evil-normal-state-map
            (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
            (define-key evil-normal-state-local-map (kbd "-") 'neotree-select-up-node)
            (define-key evil-normal-state-local-map (kbd "o") 'neotree-change-root)
         ))


(provide 'my-mode-neotree)
