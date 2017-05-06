(use-package company
  :defer 5
  :diminish company-mode
  :bind
  (:map evil-insert-state-map
   ("C-\\" . company-complete-common-or-cycle)
   :map company-active-map
   ("C-j" . company-select-next)
   ("C-k" . company-select-previous)
   )
  :config
  (progn
    (use-package company-flx :ensure t)
    (use-package company-quickhelp :ensure t)
    (global-company-mode)
    (company-flx-mode 1)))


(provide 'my-mode-company)

