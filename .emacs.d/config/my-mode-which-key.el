(use-package which-key
  :defer 1
  :diminish which-key-mode
  :init
  (progn
    (setq which-key-show-prefix nil)
    (setq which-key-sort-order 'which-key-prefix-then-key-order)))

(provide 'my-mode-which-key)
